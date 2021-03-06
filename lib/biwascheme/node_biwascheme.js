var _ = require('underscore');
_.str = require('underscore.string');

var Console = {};
Console.puts = function(str, no_newline) {
  require('util').print(str);
  if (!no_newline) {
    require('util').print("\n");
  }
};

Console.p = function() {
  require('util').print.apply(this, arguments);
};

if(typeof(ev) != 'function') {
  eval("function ev(str){ puts(str); return (new BiwaScheme.Interpreter()).evaluate(str); }");
}

if(typeof(BiwaScheme) == "undefined") BiwaScheme = {};
BiwaScheme.on_node = true;
BiwaScheme._ = _;
BiwaScheme.Console = Console;
var BiwaScheme = BiwaScheme || {};

BiwaScheme.Version = "0.6.2.dev";
BiwaScheme.GitCommit = "12a824f1271e73dcb1a7c10a02176db59a7ff853";
// 
// Heap based scheme from 3imp.pdf
//

// default definition of puts: should be overriden for console interpreters

function puts(str, no_newline){
    Console.puts(str, no_newline)
}
function p(/*args*/){
    Console.p.apply(this, arguments)
}

//
// variables
//
BiwaScheme.TopEnv = {};
BiwaScheme.CoreEnv = {};

//
// Nil
// javascript representation of empty list( '() )
//
BiwaScheme.nil = {
  toString: function() { return "nil"; },
  to_write: function() { return "()"; },
  to_array: function() { return []; },
  length: function() { return 0; }
};

//
// #<undef> (The undefined value)
// also used as #<unspecified> values
//
BiwaScheme.undef = new Object();
BiwaScheme.undef.toString = function(){ return "#<undef>"; }

// Prints the arguments to console.debug.
BiwaScheme.debug = function(/*arguments*/){
  var args = _.toArray(arguments);
  console.debug.apply(console, _.map(args, BiwaScheme.inspect));
}
//
// Super-simple class implementation
//
// Example usage:
//
// BiwaScheme.Foo = BiwaScheme.Class.create({
//   initialize: function(a){
//     this.a = a;
//   },
//
//   toString: function(){
//     return "foo[" + this.a + "]";
//   }
// });
//
// BiwaScheme.Bar = BiwaScheme.Class.extend(new BiwaScheme.Foo("hello1"), {
//   initialize: function(b){
//     this.b = b;
//   },
//
//   printEverything: function(){
//     console.log("a = ", this.a, "b = ", this.b);
//   },
//
//   toString: function(){
//     return "bar[" + this.b + "]";
//   }
// });

BiwaScheme.Class = {
  create: function(methods) {
    var klass = function(){ this.initialize.apply(this, arguments); };
    _.extend(klass.prototype, methods);
    return klass;
  },

  extend: function(parent, methods) {
    var klass = function(){ this.initialize.apply(this, arguments); };
    klass.prototype = parent;
    _.extend(klass.prototype, methods);
    return klass;
  }
};

// Update the given method to memoized version.
//
// - klass : a class defined by BiwaScheme.Class.create
// - name_or_names : method name (a string or an array of strings)
//
// Example
//
//   // Given this method
//   BiwaScheme.Enumeration.EnumType = ...
//     universe: function(){
//       return ...
//     }
//   ...
//   // Memoize
//   BiwaScheme.Class.memoize(BiwaScheme.Enumeration.EnumType,
//                            "_universe"); 
//
//   // Equal to:
//   BiwaScheme.Enumeration.EnumType = ...
//     universe: function(){
//       if(!this.hasOwnProperty("cached_universe")){
//         this.cached_universe = this.compute_universe();
//       }
//       return this.cached_universe;
//     },
//     compute_universe: function(){ 
//       // Original function, renamed to compute_*
//       return ...
//     }
//   ...
BiwaScheme.Class.memoize = function(klass, name_or_names){
  var proto = klass.prototype;
  var names = _.isArray(name_or_names) ? name_or_names : [name_or_names];

  _.each(names, function(name){
    // Copy original function foo as 'compute_foo'
    proto["compute_"+name] = proto[name];

    // Define memoizing version
    proto[name] = function(/*arguments*/){
      if(!this.hasOwnProperty("cached_"+name)){
        this["cached_"+name] = this["compute_"+name].apply(this, _.toArray(arguments));
      }
      return this["cached_"+name];
    }
  });
}
//
// write.js: Functions to convert objects to strings
//

//
// write
//

BiwaScheme.to_write = function(obj){
  if(obj === undefined)
    return "undefined";
  else if(obj === null)
    return "null";
  else if(_.isFunction(obj))
    return "#<Function "+(obj.fname ? obj.fname :
                          obj.toSource ? _.str.truncate(obj.toSource(), 40) :
                          "")+">";
  else if(_.isString(obj))
    return '"' +
           obj.replace(/\\|\"/g,function($0){return'\\'+$0;})
              .replace(/\x07/g, "\\a")
              .replace(/\x08/g, "\\b")
              .replace(/\t/g, "\\t")
              .replace(/\n/g, "\\n")
              .replace(/\v/g, "\\v")
              .replace(/\f/g, "\\f")
              .replace(/\r/g, "\\r") +
           '"';
  else if(_.isArray(obj) && obj.closure_p)
    return "#<Closure>";
  else if(_.isArray(obj))
    return "#(" + _.map(obj, function(e) { return BiwaScheme.to_write(e); }).join(" ") + ")";
  else if(typeof(obj.to_write) == 'function')
    return obj.to_write();
  else if(isNaN(obj) && typeof(obj) == 'number')
    return "+nan.0";
  else{
    switch(obj){
      case true: return "#t";
      case false: return "#f";
      case Infinity: return "+inf.0";
      case -Infinity: return "-inf.0";
    }
  }
  return BiwaScheme.inspect(obj);
}

//
// display
//

BiwaScheme.to_display = function(obj){
  if(typeof(obj.valueOf()) == "string")
    return obj;
  else if(obj instanceof BiwaScheme.Symbol)
    return obj.name;
  else if(obj instanceof Array)
    return '#(' + _.map(obj, BiwaScheme.to_display).join(' ') + ')';
  else if(obj instanceof BiwaScheme.Pair)
    return obj.inspect(BiwaScheme.to_display);
  else if(obj instanceof BiwaScheme.Char)
    return obj.value;
  else
    return BiwaScheme.to_write(obj);
}

//
// write/ss (write with substructure)
//

// example:  > (let ((x (list 'a))) (list x x))                   //           (#0=(a) #0#)
// 2-pass algorithm.
// (1) detect all the objects which appears more than once
//     (find_cyclic, reduce_cyclic_info)
// (2) write object using this information
//   * add prefix '#n=' for first appearance
//   * just write '#n#' for other appearance

//TODO: support Values
BiwaScheme.write_ss = function(obj, array_mode){
  var known = [obj], used = [false];
  BiwaScheme.find_cyclic(obj, known, used);
  var cyclic   = BiwaScheme.reduce_cyclic_info(known, used);
  var appeared = new Array(cyclic.length);
  for(var i=cyclic.length-1; i>=0; i--) appeared[i] = false;

  return BiwaScheme.to_write_ss(obj, cyclic, appeared, array_mode);
}
BiwaScheme.to_write_ss = function(obj, cyclic, appeared, array_mode){
  var ret = "";
  var i = cyclic.indexOf(obj);
  if(i >= 0){
    if(appeared[i]){
      return "#"+i+"#";
    }
    else{
      appeared[i] = true;
      ret = "#"+i+"=";
    }
  }

  if(obj instanceof BiwaScheme.Pair){
    var a = [];
    a.push(BiwaScheme.to_write_ss(obj.car, cyclic, appeared, array_mode));
    for(var o=obj.cdr; o != BiwaScheme.nil; o=o.cdr){
      if(!(o instanceof BiwaScheme.Pair) || cyclic.indexOf(o) >= 0){
        a.push(".");
        a.push(BiwaScheme.to_write_ss(o, cyclic, appeared, array_mode));
        break;
      }
      a.push(BiwaScheme.to_write_ss(o.car, cyclic, appeared, array_mode));
    }
    ret += "(" + a.join(" ") + ")";
  }
  else if(obj instanceof Array){
    var a = _.map(obj, function(item){
      return BiwaScheme.to_write_ss(item, cyclic, appeared, array_mode);
    })
    if(array_mode)
      ret += "[" + a.join(", ") + "]";
    else
      ret += "#(" + a.join(" ") + ")";
  }
  else{
    ret += BiwaScheme.to_write(obj);
  }
  return ret;
}
BiwaScheme.reduce_cyclic_info = function(known, used){
  var n_used = 0;
  for(var i=0; i<used.length; i++){
    if(used[i]){
      known[n_used] = known[i];
      n_used++;
    }
  }
  return known.slice(0, n_used);
}
BiwaScheme.find_cyclic = function(obj, known, used){
  var items = (obj instanceof BiwaScheme.Pair)  ? [obj.car, obj.cdr] :
              (obj instanceof Array) ? obj :
              null;
  if(!items) return;

  _.each(items, function(item){
    if(typeof(item)=='number' || typeof(item)=='string' ||
      item === BiwaScheme.undef || item === true || item === false ||
      item === BiwaScheme.nil || item instanceof BiwaScheme.Symbol) return;

    var i = known.indexOf(item);
    if(i >= 0)
      used[i] = true;
    else{
      known.push(item);
      used.push(false);
      BiwaScheme.find_cyclic(item, known, used);
    }
  });
};

//
// inspect
//
BiwaScheme.inspect = function(object, opts) {
  try {
    if (_.isUndefined(object)) return 'undefined';
    if (object === null) return 'null';
    if (object === true) return '#t';
    if (object === false) return '#f';
    if (object.inspect) return object.inspect();
    if (_.isString(object)) {
      return '"' + object.replace(/"/g, '\\"') + '"';
    }
    if (_.isArray(object)) {
      return '[' + _.map(object, BiwaScheme.inspect).join(', ') + ']';
    }

    if (opts && opts["fallback"]){
      return opts["fallback"];
    }
    else {
      return object.toString();
    }
  } catch (e) {
    if (e instanceof RangeError) return '...';
    throw e;
  }
};
//
// types.js - type predicators
//

BiwaScheme.isNil = function(obj){
  return (obj === BiwaScheme.nil);
};

BiwaScheme.isUndef = function(obj){
  return (obj === BiwaScheme.undef);
};

BiwaScheme.isChar = function(obj){
  return (obj instanceof BiwaScheme.Char);
};

BiwaScheme.isSymbol = function(obj){
  return (obj instanceof BiwaScheme.Symbol);
};

BiwaScheme.isTheSymbol = function(name, obj){
  return (obj instanceof BiwaScheme.Symbol) &&
         (obj.name == name);
};

BiwaScheme.isPort = function(obj){
  return (obj instanceof BiwaScheme.Port);
};

// Note: '() is not a pair in scheme
BiwaScheme.isPair = function(obj){
  return (obj instanceof BiwaScheme.Pair);
};

// Returns true if obj is '() or all the cdr's are list.
// Note: isList returns true for '()
BiwaScheme.isList = function(obj){
    if(obj === BiwaScheme.nil) return true; // null base case
    if(!(obj instanceof BiwaScheme.Pair)) return false;
    return BiwaScheme.isList(obj.cdr);
  //TODO: should check if it is not cyclic..
};

BiwaScheme.isVector = function(obj){
  return (obj instanceof Array) && (obj.closure_p !== true);
};

BiwaScheme.isHashtable = function(obj){
  return (obj instanceof BiwaScheme.Hashtable);
};

BiwaScheme.isMutableHashtable = function(obj){
  return (obj instanceof BiwaScheme.Hashtable) && obj.mutable;
};

BiwaScheme.isClosure = function(obj){
  return (obj instanceof Array) && (obj.closure_p === true);
};

// procedure: Scheme closure or JavaScript function
// valid argument for anywhere function is expected
BiwaScheme.isProcedure = function(obj){
  return BiwaScheme.isClosure(obj) || _.isFunction(obj);
};
//
// Errors
//

BiwaScheme.Error = BiwaScheme.Class.create({
  initialize: function(msg){
    this.message = "Error: "+msg;
  },
  toString: function(){
    return this.message;
  }
});

BiwaScheme.Bug = BiwaScheme.Class.extend(new BiwaScheme.Error(), {
  initialize: function(msg){
    this.message = "[BUG] "+msg;
  }
});

// currently used by "raise"
BiwaScheme.UserError = BiwaScheme.Class.extend(new BiwaScheme.Error(), {
  initialize: function(msg){
    this.message = msg;
  }
});

//
// Set - set of string
// contents must be string (or at least sortable)
//
BiwaScheme.Set = BiwaScheme.Class.create({
  initialize : function(/*args*/){
    this.arr = [];
    var i;
    for(i=0; i<arguments.length; i++)
      this.arr[i] = arguments[i];
  },

  equals : function(other){
    if(this.arr.length != other.arr.length)
      return false;

    var a1 = _.clone(this.arr);
    var a2 = _.clone(other.arr);
    a1.sort();
    a2.sort();
    for(var i=0; i<this.arr.length; i++){
      if(a1[i] != a2[i]) return false;
    }
    return true;
  },
  set_cons : function(item){
    var o = new BiwaScheme.Set(item);
    o.arr = _.clone(this.arr);
    o.arr.push(item);
    return o;
  },
  set_union : function(/*args*/){
    var o = new BiwaScheme.Set();
    o.arr = _.clone(this.arr);

    for(var k=0; k<arguments.length; k++){
      var s2 = arguments[k];
      if(!(s2 instanceof BiwaScheme.Set))
        throw new BiwaScheme.Error("set_union: arguments must be a set");

      for(var i=0; i<s2.arr.length; i++)
        o.add(s2.arr[i]);
    }
    return o;
  },
  set_intersect : function(s2){
    if(!(s2 instanceof BiwaScheme.Set))
      throw new BiwaScheme.Error("set_intersect: arguments must be a set");

    var o = new BiwaScheme.Set();
    for(var i=0; i<this.arr.length; i++)
      if(s2.member(this.arr[i]))
        o.add(this.arr[i]);
    return o;
  },
  set_minus : function(s2){
    if(!(s2 instanceof BiwaScheme.Set))
      throw new BiwaScheme.Error("set_minus: arguments must be a set");

    var o = new BiwaScheme.Set();
    for(var i=0; i<this.arr.length; i++)
      if(!s2.member(this.arr[i]))
        o.add(this.arr[i]);
    return o;
  },
  add : function(item){
    if(!this.member(item)){
      this.arr.push(item);
    }
  },
  member : function(item){
    for(var i=0; i<this.arr.length; i++)
      if(this.arr[i] == item) return true;

    return false;
  },
  rindex : function(item){
    for(var i=this.arr.length-1; i>=0 ; i--)
      if(this.arr[i] == item) return (this.arr.length-1-i);

    return null;
  },
  index : function(item){
    for(var i=0; i<this.arr.length; i++)
      if(this.arr[i] == item) return i;

    return null;
  },
  inspect : function(){
    return "Set(" + this.arr.join(", ") + ")";
  },
  toString : function(){
    return this.inspect();
  },
  size : function(){
    return this.arr.length;
  }
});
//
// Values
//
BiwaScheme.Values = BiwaScheme.Class.create({
  initialize: function(values){
    this.content = values;
  },
  to_write: function(){
    return "#<Values " +
             _.map(this.content, BiwaScheme.to_write).join(" ") +
           ">";
  }
});

//
// Pair 
// cons cell
//

BiwaScheme.Pair = BiwaScheme.Class.create({
  initialize: function(car, cdr){
    this.car = car;
    this.cdr = cdr;
  },

  caar: function(){ return this.car.car; },
  cadr: function(){ return this.cdr.car; },
  cdar: function(){ return this.cdr.car; },
  cddr: function(){ return this.cdr.cdr; },

  first:  function(){ return this.car; },
  second: function(){ return this.cdr.car; },
  third:  function(){ return this.cdr.cdr.car; },
  fourth: function(){ return this.cdr.cdr.cdr.car; },
  fifth:  function(){ return this.cdr.cdr.cdr.cdr.car; },

  // returns array containing all the car's of list
  // '(1 2 3) => [1,2,3]
  // '(1 2 . 3) => [1,2]
  to_array: function(){
    var ary = [];
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      ary.push(o.car);
    }
    return ary;
  },

  to_set: function(){
    var set = new BiwaScheme.Set();
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      set.add(o.car);
    }
    return set;
  },

  length: function(){
    var n = 0;
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      n++;
    }
    return n;
  },

  // calls the given func passing each car of list
  // returns cdr of last Pair
  foreach: function(func){
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      func(o.car);
    }
    return o;
  },

  // Returns an array which contains the resuls of calling func
  // with the car's as an argument.
  // If the receiver is not a proper list, the last cdr is ignored.
  // The receiver must not be a cyclic list.
  map: function(func){
    var ary = [];
    for(var o = this; BiwaScheme.isPair(o); o = o.cdr){
      ary.push(func(o.car));
    }
    return ary;
  },

  // Destructively concat the given list to the receiver.
  // The receiver must be a proper list.
  // Returns the receiver.
  concat: function(list){
    var o = this;
    while(o instanceof BiwaScheme.Pair && o.cdr != BiwaScheme.nil){
      o = o.cdr;
    }
    o.cdr = list;
    return this;
  },

  // returns human-redable string of pair
  inspect: function(conv){
    conv || (conv = BiwaScheme.inspect);
    var a = [];
    var last = this.foreach(function(o){
      a.push(conv(o));
    });
    if(last != BiwaScheme.nil){
      a.push(".");
      a.push(conv(last));
    }
    return "(" + a.join(" ") + ")";
  },
  toString : function(){
    return this.inspect();
  },

  to_write: function(){
    return this.inspect(BiwaScheme.to_write);
  }
});

// Creates a list out of the arguments, optionally converting any nested arrays into nested lists if the deep argument is true.
// Example:
//   BiwaScheme.List(1, 2, [3, 4]) ;=> (list 1 2 (vector 3 4))
//   BiwaScheme.deep_array_to_list(1, 2, [3, 4]) ;=> (list 1 2 (list 3 4))
var array_to_list = function(ary, deep) {
  var list = BiwaScheme.nil;
  for(var i=ary.length-1; i>=0; i--){
    var obj = ary[i];
    if(deep && _.isArray(obj) && !obj.is_vector){
      obj = array_to_list(obj, deep);
    }
    list = new BiwaScheme.Pair(obj, list);
  }
  return list;
}

// Shallow: List(1, 2, [3]) == (list 1 2 (vector 3 4))
BiwaScheme.List = function() {
  var ary = _.toArray(arguments);
  return array_to_list(ary, false);
};

// Shallow: array_to_list(1, 2, [3]) == (list 1 2 (vector 3 4))
BiwaScheme.array_to_list = function(ary) {
  return array_to_list(ary, false);
};

// Deep: deep_array_to_list(1, 2, [3, 4]) == (list 1 2 (list 3 4))
// deep_array_to_list([1, 2, 3]) - deep
BiwaScheme.deep_array_to_list = function(ary) {
  return array_to_list(ary, true);
};
//
// Symbol
//

BiwaScheme.Symbol = BiwaScheme.Class.create({
  initialize: function(str){
    this.name = str;
    BiwaScheme.Symbols[ str ] = this;
  },

  inspect: function(){
    return "'"+this.name;
    //return "#<Symbol '"+this.name+"'>";
  },

  toString: function(){
    return "'"+this.name;
  },

  to_write: function(){
    return this.name;
  }
});
BiwaScheme.Symbols = {};
BiwaScheme.Sym = function(name,leaveCase){
  if( BiwaScheme.Symbols[name] === undefined ){
    return new BiwaScheme.Symbol(name);
  }
  else if( ! (BiwaScheme.Symbols[name] instanceof BiwaScheme.Symbol) ){ //pre-defined member (like 'eval' in Firefox)
    return new BiwaScheme.Symbol(name);
  }
  else{
    return BiwaScheme.Symbols[name];
  }
}

BiwaScheme.gensym = function(){
  return BiwaScheme.Sym(_.uniqueId("__gensym"));
};
//
// Char
//

BiwaScheme.Char = BiwaScheme.Class.create({
  initialize: function(c){
    BiwaScheme.Chars[ this.value = c ] = this;
  },
  to_write: function(){
    switch(this.value){
      case '\n': return "#\\newline";
      case ' ':  return "#\\space";
      case '\t': return "#\\tab";
      default:   return "#\\"+this.value;
    }
  },
  inspect: function(){
    return this.to_write();
  }
});
BiwaScheme.Chars = {};
BiwaScheme.Char.get = function(c) {
  if(typeof(c) != "string") {
    throw new BiwaScheme.Bug("Char.get: " +
                             BiwaScheme.inspect(c) + " is not a string");
  }
  if( BiwaScheme.Chars[c] === undefined )
    return new BiwaScheme.Char(c);
  else
    return BiwaScheme.Chars[c];
};

//
// number.js - Complex and Rational
//

BiwaScheme.Complex = BiwaScheme.Class.create({
  initialize: function(real, imag){
    this.real = real;
    this.imag = imag;
  },
  magnitude: function(){
    return Math.sqrt(this.real * this.real + this.imag * this.imag);
  },
  angle: function(){
    return Math.acos(this.real / this.magnitude());
  }
})
BiwaScheme.Complex.from_polar = function(r, theta){
  var real = r * Math.cos(theta);
  var imag = r * Math.sin(theta);
  return new BiwaScheme.Complex(real, imag);
}
BiwaScheme.Complex.assure = function(num){
  if(num instanceof BiwaScheme.Complex)
    return num
  else
    return new BiwaScheme.Complex(num, 0);
}

BiwaScheme.Rational = BiwaScheme.Class.create({
  initialize: function(numerator, denominator){
    this.numerator = numerator;
    this.denominator = denominator;
  }
})


//
// Port
//

// (eof-object)
BiwaScheme.eof = new Object;

BiwaScheme.Port = BiwaScheme.Class.create({
  initialize: function(is_in, is_out){
    this.is_open = true;
    this.is_binary = false; //??
    this.is_input = is_in;
    this.is_output = is_out;
  },
  close: function(){
    // close port
    this.is_open = false;
  },
  inspect: function(){
    return "#<Port>";
  },
  to_write: function(){
    return "#<Port>";
  }
});
BiwaScheme.Port.BrowserInput = BiwaScheme.Class.extend(new BiwaScheme.Port(true, false), {
  initialize: function(){
  },
  get_string: function(after){
    var form = $("<form/>");
    form.html("<input id='webscheme-read-line' type='text'><input type='submit' value='ok'>");
    $("#bs-console").append(form);

    return new BiwaScheme.Pause(function(pause){
      form.submit(function(){
        var input = $("#webscheme-read-line").val();
        form.remove();
        puts(input);
        pause.resume(after(input));
        return false;
      });
    });
  }
})
BiwaScheme.Port.DefaultOutput = BiwaScheme.Class.extend(new BiwaScheme.Port(false, true), {
  initialize: function(){
  },
  put_string: function(str){
    puts(str, true);
  }
})

//
// string ports (srfi-6)
//
BiwaScheme.Port.StringOutput = BiwaScheme.Class.extend(new BiwaScheme.Port(false, true), {
  initialize: function(){
    this.buffer = [];
  },
  put_string: function(str){
    this.buffer.push(str);
  },
  output_string: function(str){
    return this.buffer.join("");
  }
});
BiwaScheme.Port.StringInput = BiwaScheme.Class.extend(new BiwaScheme.Port(true, false), {
  initialize: function(str){
    this.str = str;
  },
  get_string: function(after){
    return after(this.str);
  }
});
BiwaScheme.Port.current_input  = new BiwaScheme.Port.BrowserInput();
BiwaScheme.Port.current_output = new BiwaScheme.Port.DefaultOutput();
BiwaScheme.Port.current_error  = new BiwaScheme.Port.DefaultOutput();
//
// R6RS Records
// http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-7.html#node_chap_6
//
// Record is like struct in C, but supports more feature like inheritance.
// see also: src/library/r6rs_lib.js

//
// Record 
// represents each instance of record type
//
BiwaScheme.Record = BiwaScheme.Class.create({
  initialize: function(rtd, values){
    BiwaScheme.assert_record_td(rtd, "new Record");

    this.rtd = rtd;
    this.fields = values;
  },

  get: function(k){
    return this.fields[k]
  },

  set: function(k, v){
    this.fields[k] = v;
  },

  toString: function(){
    var contents = BiwaScheme.to_write(this.fields);
    return "#<Record "+this.rtd.name+" "+contents+">";
  }
});

BiwaScheme.isRecord = function(o){
  return (o instanceof BiwaScheme.Record);
};

// Defined record types
BiwaScheme.Record._DefinedTypes = {};

BiwaScheme.Record.define_type = function(name_str, rtd, cd){
  return BiwaScheme.Record._DefinedTypes[name_str] = {rtd: rtd, cd: cd};
};
BiwaScheme.Record.get_type = function(name_str){
  return BiwaScheme.Record._DefinedTypes[name_str];
};

//
// RTD (Record type descriptor)
//
BiwaScheme.Record.RTD = BiwaScheme.Class.create({
  //                   Symbol RTD        Symbol Bool  Bool    Array
  initialize: function(name, parent_rtd, uid, sealed, opaque, fields){
    this.name = name;
    this.parent_rtd = parent_rtd;
    this.is_base_type = !parent_rtd;

    if(uid){
      this.uid = uid;
      this.generative = false;
    }
    else{
      this.uid = this._generate_new_uid();;
      this.generative = true;
    }

    this.sealed = !!sealed;
    this.opaque = parent_rtd.opaque || (!!opaque);

    this.fields = _.map(fields, function(field){
      return {name: field[0], mutable: !!field[1]};
    });
  },

  // Returns the name of the k-th field.
  // Only used for error messages.
  field_name: function(k){
    var names = this._field_names();

    for(par = this.parent_rtd; par; par = par.parent_rtd){
      names = par._field_names() + names;
    }

    return names[k];
  },
  _field_names: function(){
    return _.map(this.fields, function(spec){
        return spec.name;
      });
  },

  _generate_new_uid: function(){
    return BiwaScheme.Sym(_.uniqueId("__record_td_uid"));
  },

  toString: function(){
    return "#<RecordTD "+name+">";
  }
});

BiwaScheme.Record.RTD.NongenerativeRecords = {};
BiwaScheme.isRecordTD = function(o){
  return (o instanceof BiwaScheme.Record.RTD);
};

//
// CD (Record constructor descriptor)
//
BiwaScheme.Record.CD = BiwaScheme.Class.create({
  initialize: function(rtd, parent_cd, protocol){
    this._check(rtd, parent_cd, protocol);
    this.rtd = rtd;
    this.parent_cd = parent_cd;
    if(protocol){
      this.has_custom_protocol = true;
      this.protocol = protocol;
    }
    else{
      this.has_custom_protocol = false;
      if(rtd.parent_rtd)
        this.protocol = this._default_protocol_for_derived_types();
      else
        this.protocol = this._default_protocol_for_base_types();
    }
  },

  _check: function(rtd, parent_cd, protocol){
    if(rtd.is_base_type && parent_cd)
      throw new Error("Record.CD.new: cannot specify parent cd of a base type");

    if(parent_cd && rtd.parent_rtd && (parent_cd.rtd != rtd.parent_rtd))
      throw new Error("Record.CD.new: mismatched parents between rtd and parent_cd");

    if(rtd.parent_rtd && !parent_cd && protocol)
      throw new Error("Record.CD.new: protocol must be #f when parent_cd is not given");

    if(parent_cd && parent_cd.has_custom_protocol && !protocol)
      throw new Error("Record.CD.new: protocol must be specified when parent_cd has a custom protocol");
  },
  
  _default_protocol_for_base_types: function(){
    // (lambda (p) p)
    // called with `p' as an argument
    return function(ar){
      var p = ar[0];
      BiwaScheme.assert_procedure(p, "_default_protocol/base");
      return p;
    };
  },

  _default_protocol_for_derived_types: function(){
    // (lambda (n) 
    //   (lambda (a b x y s t)
    //     (let1 p (n a b x y) (p s t))))
    // called with `n' as an argument
    var rtd = this.rtd;
    return function(ar){
      var n = ar[0];
      BiwaScheme.assert_procedure(n, "_default_protocol/n");

      var ctor = function(args){
        var my_argc = rtd.fields.length;
        var ancestor_argc = args.length - my_argc;

        var ancestor_values = args.slice(0, ancestor_argc);
        var my_values       = args.slice(ancestor_argc);

        // (n a b x y) => p
        return new BiwaScheme.Call(n, ancestor_values, function(ar){
          var p = ar[0];
          BiwaScheme.assert_procedure(p, "_default_protocol/p");

          // (p s t) => record
          return new BiwaScheme.Call(p, my_values, function(ar){
            var record = ar[0];
            BiwaScheme.assert_record(record, "_default_protocol/result");

            return record;
          });
        });
      };
      return ctor;
    };
  },

  toString: function(){
    return "#<RecordCD "+this.rtd.name+">";
  },

  record_constructor: function(){
    var arg_for_protocol = (this.parent_cd ? this._make_n([], this.rtd)
                                           : this._make_p());
    arg_for_protocol = _.bind(arg_for_protocol, this);

    return new BiwaScheme.Call(this.protocol, [arg_for_protocol], function(ar){
      var ctor = ar[0];
      BiwaScheme.assert_procedure(ctor, "record_constructor");
      return ctor;
    });
  },

  // Create the function `p' which is given to the protocol.
  _make_p: function(){
    return function(values){
      return new BiwaScheme.Record(this.rtd, values);
      // TODO: check argc 
    };
  },

  // Create the function `n' which is given to the protocol.
  // When creating an instance of a derived type,
  // _make_n is called for each ancestor rtd's.
  _make_n: function(children_values, rtd){
    var parent_cd = this.parent_cd;

    if(parent_cd){
      // called from protocol (n)
      var n = function(args_for_n){

        // called from protocol (p)
        var p = function(args_for_p){
          var values = [].concat(args_for_p[0]).concat(children_values)
          var parent_n = parent_cd._make_n(values, rtd);

          return new BiwaScheme.Call(parent_cd.protocol, [parent_n], function(ar){
            var ctor = ar[0];
            BiwaScheme.assert_procedure(ctor, "_make_n");

            return new BiwaScheme.Call(ctor, args_for_n, function(ar){
              var record = ar[0];
              BiwaScheme.assert_record(record);
              return record;
            });
          });
        };
        return p;
      };
      return n;
    }
    else{
      var n = function(my_values){
        var values = my_values.concat(children_values);
        return new BiwaScheme.Record(rtd, values);
        // TODO: check argc 
      };
      return n;
    }
  }
});

BiwaScheme.isRecordCD = function(o){
  return (o instanceof BiwaScheme.Record.CD);
};
// 
// R6RS Enumerations
// http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-15.html#node_chap_14
//
// Example
//
//   (define-enumeration color
//     (black white purple maroon)
//     color-set)
//   
//   (color black)                  ;=> 'black
//   (color purpel)                 ;=> &syntax exception
//   (enum-set->list
//     (color-set maroon white))    ;=> #<enum-set (white maroon)>

BiwaScheme.Enumeration = {};

// Represents an enum_type.
//
// Becuase there is no way to access an EnumType directly from Scheme,
// EnumType#to_write is not defined.
//
// Properties
//
// members - Array of symbols (no duplicate)
//
BiwaScheme.Enumeration.EnumType = BiwaScheme.Class.create({
  // Creates a new enum_type.
  //
  // members - Array of symbols.
  //           Symbols may be duplicate (I think you shouldn't, though :-p).
  initialize: function(members){
    this.members = _.uniq(members);
  },

  // Returns an EnumSet.
  universe: function(){
    return new BiwaScheme.Enumeration.EnumSet(this, this.members);
  }, 

  // Returns a function which map a symbol to an integer (or #f, if 
  // the symbol is out of the universe).
  // 
  // Implementation note: don't forget this.members may have duplicates.
  indexer: function(){
    // ar[0] - a symbol
    // Returns an integer or #f.
    return _.bind(function(ar){
      BiwaScheme.assert_symbol(ar[0], "(enum-set indexer)");
      var idx = _.indexOf(this.members, ar[0]);
      return (idx === -1) ? false : idx;
    }, this);
  },

  // Retuns a function which creates an enum_set from a list of
  // symbols (Symbols may be duplicate.)
  constructor: function(){
    // ar[0] - a list of symbol
    // Returns a enum_set.
    return _.bind(function(ar){
      BiwaScheme.assert_list(ar[0], "(enum-set constructor)");
      var symbols = ar[0].to_array();
      _.each(symbols, function(arg){
        BiwaScheme.assert_symbol(arg, "(enum-set constructor)");
      });

      return new BiwaScheme.Enumeration.EnumSet(this, symbols);
    }, this);
  }
});
BiwaScheme.Class.memoize(BiwaScheme.Enumeration.EnumType,
  ["universe", "indexer", "constructor"]); 

// Represents an enum_set of an enum_type.
//
// Properties
//
// enum_type - The enum_type.
// symbols   - Array of symbols (no duplicate, properly ordered)
//
BiwaScheme.Enumeration.EnumSet = BiwaScheme.Class.create({
  // Creates a new enum_set.
  //
  // enum_type - An EnumType
  // symbols   - Array of symbols.
  //
  // initialize normalizes symbols.
  //   - remove duplicates
  //   - order by universe
  initialize: function(enum_type, symbols){
    this.enum_type = enum_type;
    this.symbols = _.filter(enum_type.members, function(sym){
      return _.include(symbols, sym);
    });
  },

  // Returns a list of symbols.
  symbol_list: function(){
    return BiwaScheme.array_to_list(this.symbols); 
  },
  
  // Returns true if the enum_set includes the symbol.
  // 'symbol' is allowed to be a symbol which is not included in the universe.
  is_member: function(symbol){
    return _.include(this.symbols, symbol);
  },
  
  // Returns true if:
  // - the enum_set is a subset of the enum_set 'other', and
  // - the universe of the enum_set is a subset of 
  //   the universe of 'other'.
  // The enum_set and 'other' may belong to different enum_type.
  is_subset: function(other){
    // Check elements
    if(_.any(this.symbols, function(sym){
         return !_.include(other.symbols, sym);
       })){
      return false;
    }

    // Check universe
    if(this.enum_type === other.enum_type){
      return true;
    }
    else{
      return _.all(this.enum_type.members, function(sym){
               return _.include(other.enum_type.members, sym);
             });
    }
  },

  // Returns true if:
  //   - the enum_set contains the same set of symbols as 'other', and
  //   - universe of the enum_set contains the same set of symbols
  //     as the universe of 'other'.
  //
  // The enum_set and 'other' may belong to different enum_type.
  equal_to: function(other){
    return this.is_subset(other) && other.is_subset(this);
  },

  // Returns a enum_set which has:
  // - all the symbols included in the enum_set or the enum_set 'other'.
  // The enum_set and 'other' *must* belong to the same enum_type.
  union: function(other){
    var syms = _.filter(this.enum_type.members, _.bind(function(sym){
                 return _.include(this.symbols, sym) ||
                        _.include(other.symbols, sym);
               }, this));
    return new BiwaScheme.Enumeration.EnumSet(this.enum_type, syms);
  },

  // Returns a enum_set which has:
  // - the symbols included both in the enum_set or the enum_set 'other'.
  // The enum_set and 'other' *must* belong to the same enum_type.
  intersection: function(other){
    var syms = _.filter(this.symbols, function(sym){
                 return _.include(other.symbols, sym);
               });
    return new BiwaScheme.Enumeration.EnumSet(this.enum_type, syms);
  },

  // Returns a enum_set which has:
  // - the symbols included in the enum_set and not in the enum_set 'other'.
  // The enum_set and 'other' *must* belong to the same enum_type.
  difference: function(other){
    var syms = _.filter(this.symbols, function(sym){
                 return !_.include(other.symbols, sym);
               });
    return new BiwaScheme.Enumeration.EnumSet(this.enum_type, syms);
  },

  // Returns a enum_set which has:
  // - the symbols included in the universe but not in the enum_set.
  complement: function(){
    var syms = _.filter(this.enum_type.members, _.bind(function(sym){
                 return !_.include(this.symbols, sym);
               }, this));
    return new BiwaScheme.Enumeration.EnumSet(this.enum_type, syms);
  },

  // Returns a enum_set which has:
  // - the symbols included in the enum_set and the universe of the enum_set 'other'.
  // The enum_set and 'other' may belong to different enum_type.
  projection: function(other){
    var syms = _.filter(this.symbols, function(sym){
                 return _.include(other.enum_type.members, sym);
               });
    return new BiwaScheme.Enumeration.EnumSet(other.enum_type, syms);
  },

  // Returns a string which represents the enum_set.
  toString: function(){
    return "#<EnumSet "+BiwaScheme.inspect(this.symbols)+">";
  }
});
BiwaScheme.Class.memoize(BiwaScheme.Enumeration.EnumSet, "symbol_list");

BiwaScheme.isEnumSet = function(obj){
  return (obj instanceof BiwaScheme.Enumeration.EnumSet);
};
//
// Hashtable
//
// Based on the base JavaScript Object class, but
//  * Object takes only strings as keys
//  * R6RS hashtable needs its own hash function
// so some hacks are needed.

BiwaScheme.Hashtable = BiwaScheme.Class.create({
  initialize: function(_hash_proc, _equiv_proc, mutable){
    this.mutable = (mutable === undefined) ? true :
                   mutable ? true : false;

    this.hash_proc = _hash_proc;
    this.equiv_proc = _equiv_proc;

    // Hash (hashed) => (array of (key and value))
    this.pairs_of = {};
  },

  clear: function(){
    this.pairs_of = {};
  },

  candidate_pairs: function(hashed){
    return this.pairs_of[hashed];
  },

  add_pair: function(hashed, key, value){
    var pairs = this.pairs_of[hashed];

    if (pairs) {
      pairs.push([key, value]);
    }
    else {
      this.pairs_of[hashed] = [[key, value]];
    }
  },

  remove_pair: function(hashed, pair){
    var pairs = this.pairs_of[hashed];
    var i = pairs.indexOf(pair);
    if (i == -1){
      throw new BiwaScheme.Bug("Hashtable#remove_pair: pair not found!");
    }
    else {
      pairs.splice(i, 1); //remove 1 element from i-th index
    }
  },

  create_copy: function(mutable){
    var copy = new BiwaScheme.Hashtable(this.hash_proc, this.equiv_proc,
                                        mutable);
    // clone the pairs to copy
    _.each(_.keys(this.pairs_of), _.bind(function(hashed){
      var pairs = this.pairs_of[hashed];
      var cloned = _.map(pairs, function(pair){
        return _.clone(pair);
      });
      copy.pairs_of[hashed] = cloned;
    }, this));

    return copy;
  },

  size: function(){
    var n = 0;
    this._apply_pair(function(pair){
      n++;
    });
    return n;
  },

  keys: function(){
    return this._apply_pair(function(pair){
      return pair[0];
    });
  },

  values: function(){
    return this._apply_pair(function(pair){
      return pair[1];
    });
  },

  _apply_pair: function(func){
    var a = [];
    _.each(_.values(this.pairs_of), function(pairs){
      _.each(pairs, function(pair){
        a.push(func(pair));
      });
    });
    return a;
  },

  to_write: function(){
    return "#<Hashtable size=" + this.size() + ">";
  }
});

//
// Hash functions
//

BiwaScheme.Hashtable.equal_hash = function(ar){
  return BiwaScheme.to_write(ar[0]);
};
BiwaScheme.Hashtable.eq_hash = BiwaScheme.Hashtable.equal_hash;
BiwaScheme.Hashtable.eqv_hash = BiwaScheme.Hashtable.equal_hash;

BiwaScheme.Hashtable.string_hash = function(ar){
  return ar[0];
};

BiwaScheme.Hashtable.string_ci_hash = function(ar){
  return _.isString(ar[0]) ? ar[0].toLowerCase() : ar[0];
};

BiwaScheme.Hashtable.symbol_hash = function(ar){
  return (ar[0] instanceof BiwaScheme.Symbol) ? ar[0].name : ar[0];
};

//
// Equivalence functions
//

BiwaScheme.Hashtable.eq_equiv = function(ar){
  return BiwaScheme.eq(ar[0], ar[1]);
};

BiwaScheme.Hashtable.eqv_equiv = function(ar){
  return BiwaScheme.eqv(ar[0], ar[1]);
};
//
// Syntax
//
BiwaScheme.Syntax = BiwaScheme.Class.create({
  initialize: function(sname, func){
    this.sname = sname;
    this.func = func;
  },
  transform: function(x){
    if (!this.func){
      throw new BiwaScheme.Bug("sorry, syntax "+this.sname+
                               " is a pseudo syntax now");
    }
    return this.func(x);
  },
  inspect: function(){
    return "#<Syntax " + this.sname +">";
  }
})

// A built-in syntax did not have associated Syntax object.
// Following code installed dummy Syntax objects to built-in syntax.
BiwaScheme.TopEnv["define"] = new BiwaScheme.Syntax("define");
BiwaScheme.TopEnv["begin"]  = new BiwaScheme.Syntax("begin");
BiwaScheme.TopEnv["quote"]  = new BiwaScheme.Syntax("quote");
BiwaScheme.TopEnv["lambda"] = new BiwaScheme.Syntax("lambda");
BiwaScheme.TopEnv["if"]     = new BiwaScheme.Syntax("if");
BiwaScheme.TopEnv["set!"]   = new BiwaScheme.Syntax("set!");
  //
  // Parser 
  // copied from jsScheme - should be rewrriten (support #0=, etc)
  //
  BiwaScheme.Parser = BiwaScheme.Class.create({
    initialize: function(txt){
      this.tokens = this.tokenize(txt);
      this.i = 0;
    },

    inspect: function(){
      return [
        "#<Parser:",
        this.i, "/", this.tokens.length, " ",
        BiwaScheme.inspect(this.tokens),
        ">"
      ].join("");
    },

    tokenize: function(txt) {
      var tokens = new Array(), oldTxt=null;
      var in_srfi_30_comment = 0;

      while( txt != "" && oldTxt != txt ) {
        oldTxt = txt;
        txt = txt.replace( /^\s*(;[^\r\n]*(\r|\n|$)|#;|#\||#\\[^\w]|#?(\(|\[|{)|\)|\]|}|\'|`|,@|,|\+inf\.0|-inf\.0|\+nan\.0|\"(\\(.|$)|[^\"\\])*(\"|$)|[^\s()\[\]{}]+)/,
        function($0,$1) {
          var t = $1;

          if (t == "#|") {
            in_srfi_30_comment++;
            return "";
          }
          else if (in_srfi_30_comment > 0) {
            if ( /(.*\|#)/.test(t) ) {
              in_srfi_30_comment--;
              if (in_srfi_30_comment < 0) {
                throw new BiwaScheme.Error("Found an extra comment terminator: `|#'")
              }
              // Push back the rest substring to input stream.
              return t.substring(RegExp.$1.length, t.length);
            }
            else {
              return "";
            }
          }
          else {
            if( t.charAt(0) != ';' ) tokens[tokens.length]=t;
            return "";
          }
        } );
      }
      return tokens;
    },

    sexpCommentMarker: new Object,
    getObject: function() {
      var r = this.getObject0();

      if (r != this.sexpCommentMarker)
        return r;

      r = this.getObject();
      if (r == BiwaScheme.Parser.EOS)
        throw new BiwaScheme.Error("Readable object not found after S exression comment");

      r = this.getObject();
      return r;
    },
    
    getList: function( close ) {
      var list = BiwaScheme.nil, prev = list;
      while( this.i < this.tokens.length ) {

        this.eatObjectsInSexpComment("Input stream terminated unexpectedly(in list)");

        if( this.tokens[ this.i ] == ')' || this.tokens[ this.i ] == ']' || this.tokens[ this.i ] == '}' ) {
          this.i++; break;
        }

        if( this.tokens[ this.i ] == '.' ) {
          this.i++;
          var o = this.getObject();
          if( o != BiwaScheme.Parser.EOS && list != BiwaScheme.nil ) {
            prev.cdr = o;
          }
        } else {
            var cur = new BiwaScheme.Pair( this.getObject(), BiwaScheme.nil);
            if( list == BiwaScheme.nil ) list = cur;
            else prev.cdr = cur;
            prev = cur;
        }
      }
      return list;
    },

    getVector: function( close ) {
      var arr = new Array();
      while( this.i < this.tokens.length ) {
        
        this.eatObjectsInSexpComment("Input stream terminated unexpectedly(in vector)");
        
        if( this.tokens[ this.i ] == ')' ||
        this.tokens[ this.i ] == ']' ||
        this.tokens[ this.i ] == '}' ) { this.i++; break; }
        arr[ arr.length ] = this.getObject();
      }
      return arr;
    },

    eatObjectsInSexpComment: function(err_msg) {
      while( this.tokens[ this.i ] == '#;' ) {
        this.i++;
        if ((this.getObject() == BiwaScheme.Parser.EOS) || (this.i >= this.tokens.length))
          throw new BiwaScheme.Error(err_msg);  
      }
    }, 

    getObject0: function() {
      if( this.i >= this.tokens.length )
        return BiwaScheme.Parser.EOS;

      var t = this.tokens[ this.i++ ];
      // if( t == ')' ) return null;

      if (t == '#;')
        return this.sexpCommentMarker;

      var s = t == "'"  ? 'quote' :
              t == "`"  ? 'quasiquote' :
              t == ","  ? 'unquote' :
              t == ",@" ? 'unquote-splicing' : false;

      if( s || t == '(' || t == '#(' || t == '[' || t == '#[' || t == '{' || t == '#{' ) {
        return s ? new BiwaScheme.Pair( BiwaScheme.Sym(s), new BiwaScheme.Pair( this.getObject(), BiwaScheme.nil ))
        : (t=='(' || t=='[' || t=='{') ? this.getList(t) : this.getVector(t);
      } 
      else {
        switch(t){
          case "+inf.0" : return Infinity;
          case "-inf.0" : return -Infinity;
          case "+nan.0" : return NaN;
        }

        var n;
        if( /^#x[0-9a-z]+$/i.test(t) ) {  // #x... Hex
          n = new Number('0x'+t.substring(2,t.length) );
        } 
        else if( /^#d[0-9\.]+$/i.test(t) ) {  // #d... Decimal
          n = new Number( t.substring(2,t.length) );
        } 
        else{
          n = new Number(t);  // use constrictor as parser
        }

        if( ! isNaN(n) ) {
          return n.valueOf();
        } else if( t == '#f' || t == '#F' ) {
          return false;
        } else if( t == '#t' || t == '#T' ) {
          return true;
        } else if( t.toLowerCase() == '#\\newline' ) {
          return BiwaScheme.Char.get('\n');
        } else if( t.toLowerCase() == '#\\space' ) {
          return BiwaScheme.Char.get(' ');
        } else if( t.toLowerCase() == '#\\tab' ) {
          return BiwaScheme.Char.get('\t');
        } else if( /^#\\.$/.test(t) ) {
          return BiwaScheme.Char.get( t.charAt(2) );
        } else if( /^\"(\\(.|$)|[^\"\\])*\"?$/.test(t) ) {
          return t.replace(/(\r?\n|\\n)/g, "\n").replace( /^\"|\\(.|$)|\"$/g, function($0,$1) {
            return $1 ? $1 : '';
          } );
        } else return BiwaScheme.Sym(t);  // 2Do: validate !!
      }
    }
  });
  // indicates end of source file
  BiwaScheme.Parser.EOS = new Object();
  

///
/// Compiler
///
/// Note: macro expansion is done by Intepreter#expand

BiwaScheme.Compiler = BiwaScheme.Class.create({
  initialize: function(){
  },

  is_tail: function(x){
    return (x[0] == "return");
  },

  //free: set
  //e: env(= [locals, frees])
  //next: opc
  //ret: opc["refer_*", n, ["argument", 
  //          ["refer_*", n, ... ["argument", next]
  collect_free: function(free, e, next){
    var vars = free;
    var opc = next;
    var arr = vars.arr;
    for(var i=0; i<arr.length; i++){
      opc = this.compile_refer(arr[i], e, ["argument", opc]);
    }
    //puts("collect_free "+free.inspect()+" / "+e.inspect()+" => "+opc.inspect());
    return opc;
  },

  //x: Symbol
  //e: env [set of locals, set of frees]
  //ret: opc
  compile_refer: function(x, e, next){
    return this.compile_lookup(x, e,
             function(n){ return ["refer-local", n, next] },
             function(n){ return ["refer-free",  n, next] },
             function(sym){ return ["refer-global", sym, next] });
  },

  compile_lookup: function(x, e, return_local, return_free, return_global){
    var locals = e[0], free = e[1];
    if((n = locals.index(x)) != null){
      //puts("compile_refer:"+x.inspect()+" in "+e.inspect()+" results refer-local "+n);
      return return_local(n);
    }
    else if((n = free.index(x)) != null){
      //puts("compile_refer:"+x.inspect()+" in "+e.inspect()+" results refer-free "+n);
      return return_free(n);
    }
    else{
      var sym = x.name;
      return return_global(sym);
    }
    //throw new BiwaScheme.Error("undefined symbol `" + sym + "'");
  },

  //generate boxing code (intersection of sets & vars)
  //if no need of boxing, just returns next
  //  sets(Set): assigned variables 
  //  vars(List): used variables
  //  next(opc):
  //  ret(opc):
  make_boxes: function(sets, vars, next){
    var vars = vars;
    var n = 0;
    var a = [];
    while(vars instanceof BiwaScheme.Pair){
      if(sets.member(vars.car))
        a.push(n);
      n++;
      vars = vars.cdr;
    }
    var opc = next;
    for(var i=a.length-1; i>=0; i--)
      opc = ["box", a[i], opc];
    return opc;
  },

  // Enumerate variables which (could be assigned && included in v)
  // x: exp
  // v: set(vars)
  // ret: set
  find_sets: function(x, v){
    //puts("find_sets: " + to_write(x) + " " + to_write(v))
    var ret=null;
    if(x instanceof BiwaScheme.Symbol){
      ret = new BiwaScheme.Set();
    }
    else if(x instanceof BiwaScheme.Pair){
      switch(x.first()){
      case BiwaScheme.Sym("define"):
        var exp=x.third();
        ret = this.find_sets(exp, v);
      case BiwaScheme.Sym("begin"):
        ret = this.find_sets(x.cdr, v); //(ignores improper list)
        break;
      case BiwaScheme.Sym("quote"):
        ret = new BiwaScheme.Set();
        break;
      case BiwaScheme.Sym("lambda"):
        var vars=x.second(), body=x.cdr.cdr;
        if (vars instanceof BiwaScheme.Pair){ // (lambda (...) ...)
          ret = this.find_sets(body, v.set_minus(vars.to_set()));
        }
        else { // (lambda args ...)
          ret = this.find_sets(body, v.set_minus(new BiwaScheme.Set(vars)));
        }
        break;
      case BiwaScheme.Sym("if"):
        var testc=x.second(), thenc=x.third(), elsec=x.fourth();
        ret = this.find_sets(testc, v).set_union(
                        this.find_sets(thenc, v),
                        this.find_sets(elsec, v));
        break;
      case BiwaScheme.Sym("set!"):
        var vari=x.second(), xx=x.third();
        if(v.member(vari))
          ret = this.find_sets(xx, v).set_cons(vari);
        else
          ret = this.find_sets(xx, v);
        break;
      case BiwaScheme.Sym("call/cc"):
        var exp=x.second();
        ret = this.find_sets(exp, v);
        break;
      default:
        var set = new BiwaScheme.Set();
        for(var p=x; p instanceof BiwaScheme.Pair; p=p.cdr){
          set = set.set_union(this.find_sets(p.car, v));
        }
        ret = set;
        break;
      }
    }
    else{
      ret = new BiwaScheme.Set();
    }

    if(ret == null)
      throw new BiwaScheme.Bug("find_sets() exited in unusual way");
    else
      return ret;
  },

  // find_free(): find free variables in x
  //              these variables are collected by collect_free().
  // x: expression 
  // b: set of local vars (= variables which are not free)
  // f: set of free var candidates 
  //    (local vars of outer lambdas)
  // ret: set of free vars
  find_free: function(x, b, f){
    var ret=null;
    if(x instanceof BiwaScheme.Symbol){
      if(f.member(x))
        ret = new BiwaScheme.Set(x);
      else
        ret = new BiwaScheme.Set();
    }
    else if(x instanceof BiwaScheme.Pair){
      switch(x.first()){
      case BiwaScheme.Sym("define"):
        var exp=x.third();
        ret = this.find_free(exp, b, f);
        break;
      case BiwaScheme.Sym("begin"):
        ret = this.find_free(x.cdr, b, f); //(ignores improper list)
        break;
      case BiwaScheme.Sym("quote"):
        ret = new BiwaScheme.Set();
        break;
      case BiwaScheme.Sym("lambda"):
        var vars=x.second(), body=x.cdr.cdr;
        if (vars instanceof BiwaScheme.Pair){ // (lambda (...) ...)
          ret = this.find_free(body, b.set_union(vars.to_set()), f);
        }
        else { // (lambda args ...)
          ret = this.find_free(body, b.set_cons(vars), f);
        }
        break;
      case BiwaScheme.Sym("if"):
        var testc=x.second(), thenc=x.third(), elsec=x.fourth();
        ret = this.find_free(testc, b, f).set_union(
                        this.find_free(thenc, b, f),
                        this.find_free(elsec, b, f));
        break;
      case BiwaScheme.Sym("set!"):
        var vari=x.second(), exp=x.third();
        if(f.member(vari))
          ret = this.find_free(exp, b, f).set_cons(vari);
        else
          ret = this.find_free(exp, b, f)
        break;
      case BiwaScheme.Sym("call/cc"):
        var exp=x.second();
        ret = this.find_free(exp, b, f);
        break;
      default:
        var set = new BiwaScheme.Set();
        for(var p=x; p instanceof BiwaScheme.Pair; p=p.cdr){
          set = set.set_union(this.find_free(p.car, b, f));
        }
        ret = set;
        break;
      }
    }
    else{
      ret = new BiwaScheme.Set();
    }
    //p("find_free "+x.inspect()+" / "+b.inspect()+" => "+ret.inspect());

    if(ret == null)
      throw new BiwaScheme.Bug("find_free() exited in unusual way");
    else
      return ret;
  },

  // Returns the position of the dot pair.
  // Returns -1 if x is a proper list.
  //
  // eg. (a b . c) -> 2
  find_dot_pos: function(x){
    var idx = 0;
    for (; x instanceof BiwaScheme.Pair; x = x.cdr, ++idx)
      ;
    if (x != BiwaScheme.nil) {
      return idx;
    } else {
      return -1;
    }
  },

  last_pair: function(x){
    if (x instanceof BiwaScheme.Pair){
      for (; x.cdr instanceof BiwaScheme.Pair; x = x.cdr)
        ;
    }
    return x;
  },

  // Takes an dotted list and returns proper list.
  //
  // eg. (x y . z) -> (x y z)
  dotted2proper: function(ls){
    var nreverse = function(ls){
      var res = BiwaScheme.nil;
      for (; ls instanceof BiwaScheme.Pair; ){
        var d = ls.cdr;
        ls.cdr = res;
        res = ls;
        ls = d;
      }
      return res;
    }
    var copy_list = function(ls){
      var res = BiwaScheme.nil;
      for (; ls instanceof BiwaScheme.Pair; ls = ls.cdr){
        res = new BiwaScheme.Pair(ls.car, res);
      }
      return nreverse(res);
    }

    if (ls instanceof BiwaScheme.Pair) {
      var last = this.last_pair(ls);
      if (last instanceof BiwaScheme.Pair && last.cdr === BiwaScheme.nil){
        return ls;
      } else {
        var copied = copy_list(ls);
        this.last_pair(copied).cdr = new BiwaScheme.Pair(last.cdr, BiwaScheme.nil);
        return copied;
      }
    } else {
      return new BiwaScheme.Pair(ls, BiwaScheme.nil);
    }
  },

  // x: exp(list of symbol or integer or..)
  // e: env (= [locals, frees])
  // s: vars might be set!
  // next: opc
  // ret: opc
  compile: function(x, e, s, f, next){
    //p(x);
    var ret = null;

    while(1){
      if(x instanceof BiwaScheme.Symbol){
        // Variable reference
        // compiled into refer-(local|free|global)
        return this.compile_refer(x, e, (s.member(x) ? ["indirect", next] : next));
      }
      else if(x instanceof BiwaScheme.Pair){
        switch(x.first()){
        case BiwaScheme.Sym("define"):
          ret = this._compile_define(x, next);

          x = ret[0];
          next = ret[1];
          break;

        case BiwaScheme.Sym("begin"):
          var a = [];
          for(var p=x.cdr; p instanceof BiwaScheme.Pair; p=p.cdr)
            a.push(p.car);

          //compile each expression (in reverse order)
          var c = next;
          for(var i=a.length-1; i>=0; i--){
            c = this.compile(a[i], e, s, f, c);
          }
          return c;

        case BiwaScheme.Sym("quote"):
          if(x.length() < 2)
              throw new BiwaScheme.Error("Invalid quote: "+x.to_write());

          var obj=x.second();
          return ["constant", obj, next];

        case BiwaScheme.Sym("lambda"):
          return this._compile_lambda(x, e, s, f, next);

        case BiwaScheme.Sym("if"):
          if(x.length() < 3 || x.length() > 4)
              throw new BiwaScheme.Error("Invalid if: "+x.to_write());

          var testc=x.second(), thenc=x.third(), elsec=x.fourth();
          var thenc = this.compile(thenc, e, s, f, next);
          var elsec = this.compile(elsec, e, s, f, next);
          x    = testc;
          next = ["test", thenc, elsec];
          break;

        case BiwaScheme.Sym("set!"):
          // error-checking: should have only 3 things
          if(x.length() != 3)
              throw new BiwaScheme.Error("Invalid set!: "+x.to_write());

          var v=x.second(), x=x.third();
          var do_assign = this.compile_lookup(v, e,
            function(n){ return ["assign-local", n, next]; },
            function(n){ return ["assign-free",  n, next]; },
            function(sym){ return ["assign-global",sym, next]; }
          );
          next = do_assign;
          break;

        case BiwaScheme.Sym("call/cc"): 
          var x=x.second();
          var c = ["conti", 
                    (this.is_tail(next) ? (e[0].size() + 1) : 0), //number of args for outer lambda
                    ["argument",
                    ["constant", 1,
                    ["argument",
                      this.compile(x, e, s,f,  
                        (this.is_tail(next) ? ["shift", 1, ["tco_hinted_apply"]]
                                            : ["apply"]))]]]];
                  //note: proc for call/cc takes 1 argument (= ["apply", 1])
          return this.is_tail(next) ? c : ["frame", c, next];

        default: 
          //apply 
          //x = (func 1 2) 
          //x.car = func = '(lambda (x) ..) or Symbol
          //x.cdr = args = '(1 2)
          var func = x.car;
          var args = x.cdr;
          var c = this.compile(func, e, s,f,  
                    this.is_tail(next) ? ["shift", args.length(), ["tco_hinted_apply"]]
                                       : ["apply"]);

          // VM will push the number of arguments to the stack.
          c = this.compile(args.length(), e, s, f, ["argument", c]);
          for(var p=args; p instanceof BiwaScheme.Pair; p=p.cdr){
            c = this.compile(p.car, e, s, f, ["argument", c]);
          }
          return this.is_tail(next) ? c : ["frame", c, next];
        }
      }
      else{
        return ["constant", x, next];
      }
    }
    //p("result of " + x.inspect() + ":");
    //p(ret);
    //dump({"ret":ret, "x":x, "e":e, "s":s, "next":next, "stack":[]});
//      if(ret == null)
//        throw new BiwaScheme.Bug("compile() exited in unusual way");
//      else
//        return ret;
  },

  // Compile define.
  //
  // 0. (define) ; => error
  // 1. (define a)
  // 2. (define a 1)
  // 3. (define a 1 2) ; => error
  // 4. (define (f x) x), (define (f . a) a)
  // 5. (define 1 2) 
  //
  // Note: define may appear in lambda, let, let*, let-values,
  // let*-values, letrec, letrec*. These definitions are local to the
  // <body> of these forms.
  _compile_define: function(x, next){
    if(x.length() == 1) { // 0. (define)
      throw new BiwaScheme.Error("Invalid `define': "+x.to_write());
    }

    var first = x.cdr.car;
    var rest = x.cdr.cdr;
    
    if(first instanceof BiwaScheme.Symbol){    
      if (rest === BiwaScheme.nil) { // 1. (define a)
        x = BiwaScheme.undef;
      }
      else {
        if (rest.cdr === BiwaScheme.nil) // 2. (define a 1)
          x = rest.car;
        else // 3. (define a 1 2)
          throw new BiwaScheme.Error("Invalid `define': "+x.to_write());
      }

      BiwaScheme.TopEnv[first.name] = BiwaScheme.undef;
      next = ["assign-global", first.name, next];
    }
    else if(first instanceof BiwaScheme.Pair){ // 4. (define (f x) ...)
      // Note: define of this form may contain internal define.
      // They are handled in compilation of "lambda".

      var fname=first.car, args=first.cdr;
      var lambda = new BiwaScheme.Pair(BiwaScheme.Sym("lambda"), new BiwaScheme.Pair(args, rest));
      x = lambda;
      BiwaScheme.TopEnv[fname.name] = BiwaScheme.undef;
      next = ["assign-global", fname.name, next];
    }
    else{ // 5. (define 1 2)
      throw new BiwaScheme.Error("define: symbol or pair expected but got "+first);
    }

    return [x, next];
  },

  // Compiles various forms of "lambda".
  //
  // * (lambda (x y) ...)
  // * (lambda (x y . rest) ...)
  // * (lambda args ...)
  _compile_lambda: function(x, e, s, f, next){
    if(x.length() < 3)
      throw new BiwaScheme.Error("Invalid lambda: "+x.to_write());

    var vars = x.cdr.car;
    var body = x.cdr.cdr;

    // Handle internal defines
    var tbody = BiwaScheme.Compiler.transform_internal_define(body);
    if(BiwaScheme.isPair(tbody) &&
       BiwaScheme.isSymbol(tbody.car) &&
       tbody.car.name == "letrec*"){
      // The body has internal defines.
      // Expand letrec* macro
      var cbody = BiwaScheme.Interpreter.expand(tbody);
    }
    else{
      // The body has no internal defines.
      // Just wrap the list with begin 
      var cbody = new BiwaScheme.Pair(BiwaScheme.Sym("begin"), x.cdr.cdr);
    }

    var dotpos = this.find_dot_pos(vars);
    var proper = this.dotted2proper(vars);
    var free = this.find_free(cbody, proper.to_set(), f); //free variables
    var sets = this.find_sets(cbody, proper.to_set());    //local variables

    var do_body = this.compile(cbody,
                    [proper.to_set(), free],
                    sets.set_union(s.set_intersect(free)),
                    f.set_union(proper.to_set()),
                    ["return"]);
    var do_close = ["close", 
                     free.size(),
                     this.make_boxes(sets, proper, do_body),
                     next,
                     dotpos];
    return this.collect_free(free, e, do_close);
  },

  run: function(expr){
    return this.compile(expr, [new BiwaScheme.Set(), new BiwaScheme.Set()], new BiwaScheme.Set(), new BiwaScheme.Set(), ["halt"]);
  }
});

// Compile an expression with new compiler
BiwaScheme.Compiler.compile = function(expr, next){
  expr = BiwaScheme.Interpreter.expand(expr);
  return (new BiwaScheme.Compiler).run(expr, next);
};

// Transform internal defines to letrec*.
//
// Example
//   (let ((a 1))
//     (define (b) a)
//     (b))
//
//   (let ((a 1))
//     (letrec* ((b (lambda () a)))
//       (b)))
//
// x - expression starts with (define ...)
// 
// Returns a letrec* expression, or
// just returns x, when x does not contain definitions.
(function(){
// Returns true if x is a definition
var is_definition = function(x){
  return BiwaScheme.isPair(x) &&
         BiwaScheme.isTheSymbol("define", x.car);
  // TODO: support "begin", nested "begin", "let(rec)-syntax"
};

// Convert function definition to lambda binding
//   (define a ..)         -> (a ..)
//   (define (f) ..)       -> (f (lambda () ..))
//   (define (f x . y) ..) -> (f (lambda (x . y) ..))
//   (define (f . a) ..)   -> (f (lambda a ..))
var define_to_lambda_bind = function(def){
  var sig  = def.cdr.car;
  var body = def.cdr.cdr;

  if (BiwaScheme.isSymbol(sig)) {
    var variable = sig;

    return new BiwaScheme.Pair(variable, body);
  }
  else {
    var variable = sig.car;
    var value = new BiwaScheme.Pair(BiwaScheme.Sym("lambda"),
                  new BiwaScheme.Pair(sig.cdr, body));

    return BiwaScheme.List(variable, value);
  }
};

BiwaScheme.Compiler.transform_internal_define = function(x){
  // 1. Split x into definitions and expressions
  var defs = [], item = x;
  while (is_definition(item.car)){
    defs.push(item.car);
    item = item.cdr;
  }
  var exprs = item;

  // 2. Return x if there is no definitions
  if (defs.length == 0)
    return x;
  
  // 3. Return (letrec* <bindings> <expressions>)
  var bindings = BiwaScheme.List.apply(null, _.map(defs, define_to_lambda_bind));
  return new BiwaScheme.Pair(BiwaScheme.Sym("letrec*"),
           new BiwaScheme.Pair(bindings, exprs));
};
})();
//
// pause object (facility to stop/resume interpreting)
//
BiwaScheme.Pause = BiwaScheme.Class.create({
  //new (on_pause: javascript function calling setTimeout, Ajax.Request, ..)
  initialize: function(on_pause){
    this.on_pause = on_pause;
  },

  //save state of interpreter
  set_state: function(intp, x, f, c, s){
    this.interpreter = intp;
    this.x = x;
    this.f = f;
    this.c = c;
    this.s = s;
  },

  //call this when ready (to fire setTimeout, Ajax.Request..)
  ready: function(){
    this.on_pause(this);
  },

  //restart calculation
  resume: function(value){
    return this.interpreter.resume(true, value, this.x, this.f, this.c, this.s)
  }
});

///
/// Call
///

// The class Call is used to invoke scheme closure from 
// library functions.
//
// Call#initialize takes three arguments: proc, args and after.
//   * proc is the scheme closure to invoke.
//   * args is an Array (not list!) of arguments for the invocation.
//   * after is a javascript function which is invoked when 
//     returned from the proc.
//
//     after takes two arguments: ar and intp.
//       * ar is an Array which contains the result of the invocation.
//       * intp is an Interpreter which is running.
//
//     If after returns another Call object, another invocation
//     happens. If after returns a normal value, it is the value
//     of the library function.
//
// example:
//   return new Call(proc, [x, y], function(ar){ ar[0] });
//
BiwaScheme.Call = BiwaScheme.Class.create({
  initialize: function(proc, args, after){
    this.proc = proc;
    this.args = args;
    this.after = after || function(ar){
      // just return result which closure returned
      return ar[0];
    };
  },

  inspect: function(){
    return "#<Call args=" + this.args.inspect() + ">";
  },

  toString: function(){
    return "#<Call>";
  },

  to_write: function(){
    return "#<Call>";
  }
})

//
// Iterator - external iterator for Call.foreach
//
BiwaScheme.Iterator = {
  ForArray: BiwaScheme.Class.create({
    initialize: function(arr){
      this.arr = arr;
      this.i = 0;
    },
    has_next: function(){
      return this.i < this.arr.length;
    },
    next: function(){
      return this.arr[this.i++];
    }
  }),
  ForString: BiwaScheme.Class.create({
    initialize: function(str){
      this.str = str;
      this.i = 0;
    },
    has_next: function(){
      return this.i < this.str.length;
    },
    next: function(){
      return BiwaScheme.Char.get(this.str.charAt(this.i++));
    }
  }),
  ForList: BiwaScheme.Class.create({
    initialize: function(ls){
      this.ls = ls;
    },
    has_next: function(){
      return (this.ls instanceof BiwaScheme.Pair) &&
             this.ls != BiwaScheme.nil;
    },
    next: function(){
      var pair = this.ls;
      this.ls = this.ls.cdr;
      return pair;
    }
  }),
  ForMulti: BiwaScheme.Class.create({
    initialize: function(objs){
      this.objs = objs;
      this.size = objs.length;
      this.iterators = _.map(objs, function(x){
        return BiwaScheme.Iterator.of(x);
      })
    },
    has_next: function(){
      for(var i=0; i<this.size; i++)
        if(!this.iterators[i].has_next())
          return false;
      
      return true;
    },
    next: function(){
      return _.map(this.iterators, function(ite){
        return ite.next();
      })
    }
  }),
  of: function(obj){
    switch(true){
      case (obj instanceof Array):
        return new this.ForArray(obj);
      case (typeof(obj) == "string"):
        return new this.ForString(obj);
      case (obj instanceof BiwaScheme.Pair):
      case (obj === BiwaScheme.nil):
        return new this.ForList(obj);
      default:
        throw new BiwaScheme.Bug("Iterator.of: unknown class: "+BiwaScheme.inspect(obj));
    }
  }
}

//
// Call.foreach - shortcut for successive Calls
//
// Some library functions, such as for-each or map,
// call a closure for each element. Call.foreach is 
// a utility to help defining such methods.
//
// Call.foreach takes a sequence and some callbacks.
// Sequence is an Array, String, or list.
//
// Example:
//   return Call.foreach(sequence, {
//     // before each call
//     call: function(elem){
//       return new Call(proc, [elem]);
//     },
//     // after each call
//     result: function(value, elem){
//       ary.push(value);
//       // you can return a value to terminate the loop
//     },
//     // after all the calls
//     finish: function(){
//       return ary;
//     }
//   });

BiwaScheme.Call.default_callbacks = {
  call: function(x){ return new BiwaScheme.Call(this.proc, [x]) },
  result: function(){},
  finish: function(){}
}
BiwaScheme.Call.foreach = function(obj, callbacks, is_multi){
  is_multi || (is_multi = false);
  _.each(["call", "result", "finish"], function(key){
    if(!callbacks[key])
      callbacks[key] = BiwaScheme.Call.default_callbacks[key];
  })
  
  var iterator = null;
  var x = null;

  var loop = function(ar){
    if(iterator){
      var ret = callbacks["result"](ar[0], x);
      if(ret !== undefined) return ret;
    }
    else{ // first lap
      if(is_multi)
        iterator = new BiwaScheme.Iterator.ForMulti(obj);
      else
        iterator = BiwaScheme.Iterator.of(obj);
    }

    if(!iterator.has_next()){
      return callbacks["finish"]();
    }
    else{
      x = iterator.next();
      var result = callbacks["call"](x);
      result.after = loop;
      return result;
    }
  }
  return loop(null);
}
BiwaScheme.Call.multi_foreach = function(obj, callbacks){
  return BiwaScheme.Call.foreach(obj, callbacks, true);
}

///
/// Interpreter
///

BiwaScheme.Interpreter = BiwaScheme.Class.create({
  // new BiwaScheme.Interpreter()
  // new BiwaScheme.Interpreter(lastInterpreter)
  // new BiwaScheme.Interpreter(errorHandler)
  // new BiwaScheme.Interpreter(lastInterpreter, errorHandler)
  initialize: function(){
    var last_interpreter = null;
    var on_error = null;
    if (arguments.length == 2) {
      last_interpreter = arguments[0];
      on_error = arguments[1];
    } else if (arguments.length == 1 && arguments[0] instanceof BiwaScheme.Interpreter) {
      last_interpreter = arguments[0];
    } else if (arguments.length == 1 && typeof(arguments[0]) == "function") {
      on_error = arguments[0];
    }

    this.stack = [] //(make-vector 1000)
    this.on_error = on_error || (last_interpreter ? last_interpreter.on_error : function(e){});
    this.after_evaluate = function(){};
    this.last_refer = last_interpreter ? last_interpreter.last_refer : null;
    this.call_stack = last_interpreter ? _.clone(last_interpreter.call_stack) : [];
    this.tco_counter = [];
  },

  inspect: function(){
    return [
      "#<Interpreter: stack size=>",
      this.stack.length, " ",
      "after_evaluate=",
      BiwaScheme.inspect(this.after_evaluate),
      ">"
    ].join("");
  },

  push: function(x, s){
    this.stack[s] = x;
    return s+1;
  },

  //s: depth of stack to save
  //ret: saved(copied) stack 
  save_stack: function(s){
    var v = [];
    for(var i=0; i<s; i++){
      v[i] = this.stack[i];
    }
    return { stack: v, last_refer: this.last_refer, call_stack: _.clone(this.call_stack), tco_counter: _.clone(this.tco_counter) };
  },

  //v: stack array to restore
  //ret: lenght of restored stack
  restore_stack: function(stuff){
    v = stuff.stack;
    var s = v.length;
    for(var i=0; i<s; i++){
      this.stack[i] = v[i];
    }
    this.last_refer = stuff.last_refer;
    this.call_stack = _.clone(stuff.call_stack);
    this.tco_counter = _.clone(stuff.tco_counter);
    return s;
  },

  //s: depth of stack to save
  //n: number of args(for outer lambda) to remove (= 0 unless tail position)
  //ret: closure array
  continuation: function(s, n){
    // note: implementation of this function for final version doesn't exist in 3imp.pdf..
    var ss = this.push(n, s);
    return this.closure(["refer-local", 0,
                          ["nuate", this.save_stack(ss), 
                          ["return"]]], 
                        0,     //n (number of frees)
                        null,  //s (stack position to get frees)
                        -1);   // dotpos
  },

  // shift stack 
  // n: number of items to skip (from stack top)
  // m: number of items to shift
  // s: stack pointer (= index of stack top + 1)
  shift_args: function(n, m, s){
    for(var i = n-1; i >= -1; i--){
      this.index_set(s, i+m+1, this.index(s, i));
    }
    return s-m-1;
  },

  index: function(s, i){
    return this.stack[s-i-2];
  },

  index_set: function(s, i, v){
    this.stack[s-i-2] = v;
  },

  //ret: [body, stack[s-1], stack[s-2], .., stack[s-n], dotpos]
  closure: function(body, n, s, dotpos){
    var v = []; //(make-vector n+1+1)
    v[0] = body;
    for(var i=0; i<n; i++)
      v[i+1] = this.index(s, i-1);
    v[n+1] = dotpos;

    v.closure_p = true;

    return v;
  },

  execute: function(a, x, f, c, s){
    var ret = null;
    try{
      ret = this._execute(a, x, f, c, s);
    }
    catch(e){
      e.message = e.message + " [" + this.call_stack.join(", ") + "]";
      var state = {a:a, x:x, f:f, c:c, s:s, stack:this.stack};
      return this.on_error(e, state);
    }
    return ret;
  },

  run_dump_hook: function(a, x, f, c, s) {
    var dumper;
    var state;


    if (this.dumper) {
      dumper = this.dumper;
    }
    else if (BiwaScheme.Interpreter.dumper) {
      dumper = BiwaScheme.Interpreter.dumper;
    }
    else
      return;

    if (dumper) {
      state = {"a":a,
               "f":f,
               "c":c,
               "s":s,
               "x":x,
               "stack":this.stack};
      dumper.dump(state);
    }
  },

  _execute: function(a, x, f, c, s){
    var ret = null;
    //puts("executing "+x[0]);
    
    while(true){ //x[0] != "halt"){

      this.run_dump_hook(a, x, f, c, s);

      switch(x[0]){
      case "halt":
        return a;
      case "refer-local":
        var n=x[1], x=x[2];
        a = this.index(f, n);
        this.last_refer = "(anon)";
        break;
      case "refer-free":
        var n=x[1], x=x[2];
        a = c[n+1];
        this.last_refer = "(anon)";
        break;
      case "refer-global":
        var sym=x[1], x=x[2];
        if(BiwaScheme.TopEnv.hasOwnProperty(sym))
          var val = BiwaScheme.TopEnv[sym];
        else if(BiwaScheme.CoreEnv.hasOwnProperty(sym))
          var val = BiwaScheme.CoreEnv[sym];
        else
          throw new BiwaScheme.Error("execute: unbound symbol: "+BiwaScheme.inspect(sym));

        a = val;
        this.last_refer = sym || "(anon)";
        break;
      case "indirect":
        var x=x[1];
        a = a[0]; //unboxing
        break;
      case "constant":
        var obj=x[1], x=x[2];
        a = obj;
        this.last_refer = "(anon)";
        break;
      case "close":
        var ox=x;
        var n=ox[1], body=ox[2], x=ox[3], dotpos=ox[4];
        a = this.closure(body, n, s, dotpos);
        s -= n;
        break;
      case "box":
        var n=x[1], x=x[2];
        this.index_set(s, n, [this.index(s, n)]); //boxing
        break;
      case "test":
        var thenc=x[1], elsec=x[2];
        x = ((a!==false) ? thenc : elsec);
        break;
      case "assign-global":
        var name=x[1], x=x[2];
        if(!BiwaScheme.TopEnv.hasOwnProperty(name) &&
           !BiwaScheme.CoreEnv.hasOwnProperty(name))
          throw new BiwaScheme.Error("global variable '"+name+"' is not defined");
        
        BiwaScheme.TopEnv[name] = a;
        a = BiwaScheme.undef;
        break;
      case "assign-local":
        var n=x[1], x=x[2];
        var box = this.index(f, n);
        box[0] = a;
        a = BiwaScheme.undef;
        break;
      case "assign-free":
        var n=x[1], x=x[2];
        var box = c[n+1];
        box[0] = a;
        a = BiwaScheme.undef;
        break;
      case "conti":
        var n=x[1], x=x[2];
        a = this.continuation(s, n);
        break;
      case "nuate":
        var stack=x[1], x=x[2];
        s = this.restore_stack(stack);
        break;
      case "frame":
        var ret = x[2];
        x = x[1];
        s = this.push(ret, this.push(f, this.push(c, s)));
        this.tco_counter[this.tco_counter.length] = 0;
        break;
      case "argument":
        var x=x[1];
        s = this.push(a, s);
        break;
      case "shift":
        var n=x[1], x=x[2];

        // the number of arguments in the last call
        var n_args = this.index(s, n);  

        s = this.shift_args(n, n_args, s);
        break;
      case "tco_hinted_apply": // just like a regular apply, except we need to trace the # of TCO calls so we can generate a stacktrace
        this.tco_counter[this.tco_counter.length - 1]++;
        x = ["apply"].concat(_.rest(x));
        break;
      case "apply": //extended: n_args as second argument
        var func = a; //, n_args = x[1];
        this.call_stack.push(this.last_refer);

        // the number of arguments in the last call is
        // pushed to the stack.
        var n_args = this.index(s, -1);
        if(func instanceof Array){ //closure
          a = func;
          x = func[0];

          // The position of dot in the parameter list.
          var dotpos = func[func.length-1];

          if (dotpos >= 0) {
            // The dot is found
            // ----------------
            // => Process the &rest args: packing the rest args into a list.
            var ls = BiwaScheme.nil;
            for (var i=n_args; --i>=dotpos; ) {
              ls = new BiwaScheme.Pair(this.index(s, i), ls);
            }
            if (dotpos >= n_args) {
              // No rest argument is passed to this closure.
              // However, the closure expects the caller passes the rest argument.
              // In such case this VM prepares an empty list as the rest argument.
              // --------------------------------------------------------------
              // => We extend the stack to put the empty list.
              for(var i = -1; i < n_args; i++){
                this.index_set(s, i-1, this.index(s, i));
              }
              s++;
              // => Update the number of arguments
              this.index_set(s, -1, this.index(s, -1) + 1);  
            }
            this.index_set(s, dotpos, ls);
          }
          f = s;
          c = a;
        }
        else if(func instanceof Function){ // Apply JavaScript function
          // load arguments from stack
          var args = [];
          for(var i=0; i<n_args; i++) 
            args.push(this.index(s, i));

          // invoke the function
          var result = func(args, this);

          if(result instanceof BiwaScheme.Pause){
            // it requested the interpreter to suspend
            var pause = result;
            pause.set_state(this, ["return"], f, c, s);
            pause.ready();
            return pause;
          }
          else if(result instanceof BiwaScheme.Call){
            // it requested the interpreter to call a scheme closure

            //   [frame,
            //     [constant... (args)
            //     [constant, proc
            //     [apply]]]]
            //   [frame,
            //     [constant, after
            //     [apply 1]]]]
            //   x
            var call_after = ["frame",
                               ["argument",
                               ["constant", 1,
                               ["argument",
                               ["constant", result.after,
                               ["apply"]]]]],
                             ["return"]];
            var call_proc = ["constant", result.args.length,
                            ["argument",
                            ["constant", result.proc, 
                            ["apply", result.args.length]]]];
            var push_args = _.inject(result.args, function(opc, arg){
              // (foo 1 2) => first push 2, then 1
              //   [constant 2 ... [constant 1 ... ]
              return ["constant", arg, 
                     ["argument",
                     opc]];
            }, call_proc);
            x = ["frame",
                  push_args,
                call_after]
          }
          else{
            // the JavaScript function returned a normal value
            a = result;
            x = ["return"];
          }
        }
        else{
          // unknown function type
          throw new BiwaScheme.Error(BiwaScheme.inspect(func) + " is not a function");
        }
        break;
      case "return":
        var n=this.index(s, -1);
        var ss=s-n;
        x = this.index(ss, 0),
        f = this.index(ss, 1),
        c = this.index(ss, 2),
        s = ss-3-1;
        var num_call_stack_pops = 1 + this.tco_counter[this.tco_counter.length - 1];
        _.times(num_call_stack_pops, _.bind(function() { this.call_stack.pop() }, this));
        this.tco_counter.pop();
        break;
      default:
        throw new BiwaScheme.Bug("unknown opecode type: "+x[0]);
      }
    }

//      if(ret === null)
//        throw new BiwaScheme.Bug("interpreter exited in unusual way");
//      else
//        return ret;
    return a
  },

  evaluate: function(str, after_evaluate){
    this.parser = new BiwaScheme.Parser(str);
    this.compiler = new BiwaScheme.Compiler();
    if(after_evaluate) 
      this.after_evaluate = after_evaluate;

    if(BiwaScheme.Debug) puts("executing: " + str);
     
    this.is_top = true;
    this.file_stack = [];
    return this.resume(false);
  },

  resume: function(is_resume, a, x, f, c, s){
    var ret = BiwaScheme.undef;

    for(;;){
      if(is_resume){
        ret = this.execute(a, x, f, c, s);
        is_resume = false;
      }
      else{
        if(!this.parser) break; // adhoc: when Pause is used via invoke_closure
        var expr = this.parser.getObject();
        if(expr === BiwaScheme.Parser.EOS) break;

        // expand
        expr = BiwaScheme.Interpreter.expand(expr);

        // compile
        var opc = this.compiler.run(expr);
        //if(BiwaScheme.Debug) p(opc);

        // execute
        ret = this.execute(expr, opc, 0, [], 0);
      }

      if(ret instanceof BiwaScheme.Pause){ //suspend evaluation
        return ret;
      }
    }

    // finished executing all forms
    this.after_evaluate(ret);
    return ret;
  },

  invoke_closure: function(closure, args){
    args || (args = []);
    var n_args  = args.length;

    var x = ["constant", n_args, ["argument", ["constant", closure, ["apply"]]]]
    for(var i=0; i<n_args; i++)
      x = ["constant", args[i], ["argument", x]]

    return this.execute(closure, ["frame", x, ["halt"]], 0, closure, 0);
  },

  // only compiling (for debug use only)
  compile: function(str){
    var obj = BiwaScheme.Interpreter.read(str);
    var opc = BiwaScheme.Compiler.compile(obj);
    return opc;
  }
});

// Take a string and returns an expression.
BiwaScheme.Interpreter.read = function(str){
  var parser = new BiwaScheme.Parser(str);
  var r      = parser.getObject();
  return (r == BiwaScheme.Parser.EOS)? BiwaScheme.eof: r;
};

// Expand macro calls in a expression recursively.
//
// x - expression
// flag - used internally. do not specify this
BiwaScheme.Interpreter.expand = function(x, flag/*optional*/){
  var expand = BiwaScheme.Interpreter.expand;
  flag || (flag = {})
  var ret = null;

  if(x instanceof BiwaScheme.Pair){
    switch(x.car){
    case BiwaScheme.Sym("define"):
      var left = x.cdr.car, exp = x.cdr.cdr;
      ret = new BiwaScheme.Pair(BiwaScheme.Sym("define"),
              new BiwaScheme.Pair(left, expand(exp, flag)));
      break;
    case BiwaScheme.Sym("begin"):
      ret = new BiwaScheme.Pair(BiwaScheme.Sym("begin"), expand(x.cdr, flag));
      break;
    case BiwaScheme.Sym("quote"):
      ret = x;
      break;
    case BiwaScheme.Sym("lambda"):
      var vars=x.cdr.car, body=x.cdr.cdr;
      ret = new BiwaScheme.Pair(BiwaScheme.Sym("lambda"),
              new BiwaScheme.Pair(vars, expand(body, flag)));
      break;
    case BiwaScheme.Sym("if"):
      var testc=x.second(), thenc=x.third(), elsec=x.fourth();
      if (elsec == BiwaScheme.inner_of_nil){
        elsec = BiwaScheme.undef;
      }
      ret = BiwaScheme.List(BiwaScheme.Sym("if"),
                            expand(testc, flag),
                            expand(thenc, flag),
                            expand(elsec, flag));
      break;
    case BiwaScheme.Sym("set!"):
      var v=x.second(), x=x.third();
      ret = BiwaScheme.List(BiwaScheme.Sym("set!"), v, expand(x, flag));
      break;
    case BiwaScheme.Sym("call-with-current-continuation"): 
    case BiwaScheme.Sym("call/cc"): 
      var x=x.second();
      ret = BiwaScheme.List(BiwaScheme.Sym("call/cc"), expand(x, flag));
      break;
    default: //apply
      var transformer = null;
      if(BiwaScheme.isSymbol(x.car)){
        if(BiwaScheme.TopEnv[x.car.name] instanceof BiwaScheme.Syntax)
          transformer = BiwaScheme.TopEnv[x.car.name];
        else if(BiwaScheme.CoreEnv[x.car.name] instanceof BiwaScheme.Syntax)
          transformer = BiwaScheme.CoreEnv[x.car.name];
      }

      if(transformer){
        flag["modified"] = true;
        ret = transformer.transform(x);

//            // Debug
//            var before = BiwaScheme.to_write(x);
//            var after = BiwaScheme.to_write(ret);
//            if(before != after){
//              console.log("before: " + before)
//              console.log("expand: " + after)
//            }

        var fl;
        for(;;){
          ret = expand(ret, fl={});
          if(!fl["modified"]) 
            break;
        }
      }
      else{
        var expanded_car = expand(x.car, flag);
        var expanded_cdr = BiwaScheme.array_to_list(
                             _.map(x.cdr.to_array(),
                                   function(item){ return expand(item, flag); }));
        ret = new BiwaScheme.Pair(expanded_car, expanded_cdr);
      }
    }
  }
  else{
    ret = x;
  }
  return ret;
};
///
/// infra.js - Basis for library functions
///

//
// define_*func - define library functions
//
BiwaScheme.check_arity = function(len, min, max){
  var fname = arguments.callee.caller
                ? arguments.callee.caller.fname
                : "(?)";
  if(len < min){
    if(max && max == min)
      throw new BiwaScheme.Error(fname+": wrong number of arguments (expected: "+min+" got: "+len+")");
    else
      throw new BiwaScheme.Error(fname+": too few arguments (at least: "+min+" got: "+len+")");
  }
  else if(max && max < len)
    throw new BiwaScheme.Error(fname+": too many arguments (at most: "+max+" got: "+len+")");
}
BiwaScheme.define_libfunc = function(fname, min, max, func){
  var f = function(ar, intp){
    BiwaScheme.check_arity(ar.length, min, max);
    return func(ar, intp);
  };

  func["fname"] = fname; // for assert_*
  f["fname"]    = fname; // for check_arity
  f["inspect"] = function(){ return this.fname; }
  BiwaScheme.CoreEnv[fname] = f;
}
BiwaScheme.alias_libfunc = function(fname, aliases) {
  if (BiwaScheme.CoreEnv[fname]) {
    if (_.isArray(aliases)) {
      _.map(aliases, function(a) { BiwaScheme.alias_libfunc(fname, a); });
    } else if (_.isString(aliases)) {
      BiwaScheme.CoreEnv[aliases] = BiwaScheme.CoreEnv[fname];
    } else {
      throw new BiwaScheme.Bug("bad alias for library function " +
                               "`" + fname + "': " + aliases.toString());
    }
  } else {
    throw new BiwaScheme.Bug("library function " +
                             "`" + fname + "'" +
                             " does not exist, so can't alias it.");
  }
};
BiwaScheme.define_syntax = function(sname, func) {
  var s = new BiwaScheme.Syntax(sname, func);
  BiwaScheme.CoreEnv[sname] = s;
}
BiwaScheme.define_scmfunc = function(fname, min, max, str){
  (new Interpreter).evaluate("(define "+fname+" "+str+"\n)");
}

//  define_scmfunc("map+", 2, null, 
//    "(lambda (proc ls) (if (null? ls) ls (cons (proc (car ls)) (map proc (cdr ls)))))");

//
// assertions - type checks
//
BiwaScheme.make_assert = function(check){
  return function(/*args*/){
    var fname = arguments.callee.caller
                  ? arguments.callee.caller.fname
                  : "";
    check.apply(this, [fname].concat(_.toArray(arguments)));
  }
}
BiwaScheme.make_simple_assert = function(type, test, _fname){
  return BiwaScheme.make_assert(function(fname, obj, opt){
    if(_fname) fname = _fname;
    option = opt ? ("("+opt+")") : ""
    if(!test(obj)){
      throw new BiwaScheme.Error(fname + option + ": " +
                                 type + " required, but got " +
                                 BiwaScheme.to_write(obj));
    }
  })
}

BiwaScheme.assert_number = BiwaScheme.make_simple_assert("number", function(obj){
  return typeof(obj) == 'number' || (obj instanceof BiwaScheme.Complex);
});

BiwaScheme.assert_integer = BiwaScheme.make_simple_assert("integer", function(obj){
  return typeof(obj) == 'number' && (obj % 1 == 0)
});

BiwaScheme.assert_real = BiwaScheme.make_simple_assert("real number", function(obj){
  return typeof(obj) == 'number';
});

BiwaScheme.assert_between = BiwaScheme.make_assert(function(fname, obj, from, to){
  if( typeof(obj) != 'number' || obj != Math.round(obj) ){
    throw new BiwaScheme.Error(fname + ": " +
                               "number required, but got " +
                               BiwaScheme.to_write(obj));
  }

  if( obj < from || to < obj ){
    throw new BiwaScheme.Error(fname + ": " +
                               "number must be between " +
                               from + " and " + to + ", but got " +
                               BiwaScheme.to_write(obj));
  }
});

BiwaScheme.assert_string = BiwaScheme.make_simple_assert("string", _.isString);

BiwaScheme.assert_char = BiwaScheme.make_simple_assert("character", BiwaScheme.isChar);
BiwaScheme.assert_symbol = BiwaScheme.make_simple_assert("symbol", BiwaScheme.isSymbol);
BiwaScheme.assert_port = BiwaScheme.make_simple_assert("port", BiwaScheme.isPort);
BiwaScheme.assert_pair = BiwaScheme.make_simple_assert("pair", BiwaScheme.isPair);
BiwaScheme.assert_list = BiwaScheme.make_simple_assert("list", BiwaScheme.isList);
BiwaScheme.assert_vector = BiwaScheme.make_simple_assert("vector", BiwaScheme.isVector);

BiwaScheme.assert_hashtable = BiwaScheme.make_simple_assert("hashtable",
                                          BiwaScheme.isHashtable);
BiwaScheme.assert_mutable_hashtable = BiwaScheme.make_simple_assert("mutable hashtable",
                                            BiwaScheme.isMutableHashtable);

BiwaScheme.assert_record = BiwaScheme.make_simple_assert("record",
                                          BiwaScheme.isRecord);
BiwaScheme.assert_record_td = BiwaScheme.make_simple_assert("record type descriptor",
                                          BiwaScheme.isRecordTD);
BiwaScheme.assert_record_cd = BiwaScheme.make_simple_assert("record constructor descriptor",
                                          BiwaScheme.isRecordCD);
BiwaScheme.assert_enum_set = BiwaScheme.make_simple_assert("enum_set",
                                          BiwaScheme.isEnumSet);

BiwaScheme.assert_function = BiwaScheme.make_simple_assert("JavaScript function",
                                         _.isFunction);
BiwaScheme.assert_closure = BiwaScheme.make_simple_assert("scheme function",
                                        BiwaScheme.isClosure);
BiwaScheme.assert_procedure = BiwaScheme.make_simple_assert("scheme/js function", function(obj){
  return BiwaScheme.isClosure(obj) || _.isFunction(obj);
});

BiwaScheme.assert_date = BiwaScheme.make_simple_assert("date", function(obj){
  // FIXME: this is not accurate (about cross-frame issue)
  // https://prototype.lighthouseapp.com/projects/8886/tickets/443
  return obj instanceof Date;
});

//var assert_instance_of = BiwaScheme.make_assert(function(fname, type, obj, klass){
//  if(!(obj instanceof klass)){
//    throw new BiwaScheme.Error(fname + ": " +
//                               type + " required, but got " +
//                               BiwaScheme.to_write(obj));
//  }
//});

BiwaScheme.assert = BiwaScheme.make_assert(function(fname, success, message, _fname){
  if(!success){
    throw new BiwaScheme.Error((_fname || fname)+": "+message);
  }
});


//
// R6RS Base library
//

if( typeof(BiwaScheme)!='object' ) BiwaScheme={}; with(BiwaScheme) {
  /* --------------------------------------- namespace webscheme */ 

  ///
  /// R6RS Base library
  ///
  
  //
  //        11.4  Expressions
  //
  //            11.4.1  Quotation
  //(quote)
  //            11.4.2  Procedures
  //(lambda)
  //            11.4.3  Conditionaar
  //(if)
  //            11.4.4  Assignments
  //(set!)
  //            11.4.5  Derived conditionaar

  define_syntax("cond", function(x){
    var clauses = x.cdr;
    if(!(clauses instanceof Pair) || clauses === nil){
      throw new Error("malformed cond: cond needs list but got " +
                      to_write_ss(clauses));
    }
    // TODO: assert that clauses is a proper list

    var ret = null;
    _.each(clauses.to_array().reverse(), function(clause){
      if(!(clause instanceof Pair)){
        throw new Error("bad clause in cond: " + to_write_ss(clause));
      }

      if(clause.car === Sym("else")){ 
        if(ret !== null){
          throw new Error("'else' clause of cond followed by more clauses: " +
                          to_write_ss(clauses));
        }
        else if(clause.cdr === nil){
          // pattern A: (else)
          //  -> #f            ; not specified in R6RS...?
          ret = false;
        }
        else if(clause.cdr.cdr === nil){
          // pattern B: (else expr) 
          //  -> expr
          ret = clause.cdr.car;
        }
        else{
          // pattern C: (else expr ...)
          //  -> (begin expr ...)
          ret = new Pair(Sym("begin"), clause.cdr);
        }
      }
      else{
        var test = clause.car;
        if(clause.cdr === nil){
          // pattern 1: (test)
          //  -> (or test ret)
          ret = List(Sym("or"), test, ret);
        }
        else if (clause.cdr.cdr === nil){
          // pattern 2: (test expr)
          //  -> (if test expr ret)
          ret = List(Sym("if"), test, clause.cdr.car, ret);
        }
        else if(clause.cdr.car === Sym("=>")){
          // pattern 3: (test => expr)
          //  -> (let ((#<gensym1> test)) 
          //       (if test (expr #<gensym1>) ret))
          var test = clause.car, expr = clause.cdr.cdr.car;
          var tmp_sym = BiwaScheme.gensym();

          ret = List(Sym("let"), 
                     List( List(tmp_sym, test) ),
                     List(Sym("if"), test, List(expr, tmp_sym), ret));
        }
        else{
          // pattern 4: (test expr ...)
          //  -> (if test (begin expr ...) ret)
          ret = List(Sym("if"), test,
                     new Pair(Sym("begin"), clause.cdr),
                     ret);
        }
      }
    });
    return ret;
  });

  define_syntax("case", function(x){
    var tmp_sym = BiwaScheme.gensym();

    if(x.cdr === nil){
      throw new Error("case: at least one clause is required");
    }
    else if(!(x.cdr instanceof Pair)){
      throw new Error("case: proper list is required");
    }
    else{
      // (case key clauses ....)
      //  -> (let ((#<gensym1> key))
      var key = x.cdr.car;
      var clauses = x.cdr.cdr;

      var ret = undefined;
      _.each(clauses.to_array().reverse(), function(clause){
        if(clause.car === Sym("else")){
          // pattern 0: (else expr ...)
          //  -> (begin expr ...)
          if(ret === undefined){
            ret = new Pair(Sym("begin"), clause.cdr);
          }
          else{
            throw new Error("case: 'else' clause followed by more clauses: " +
                            to_write_ss(clauses));
          }
        }
        else{
          // pattern 1: ((datum ...) expr ...)
          //  -> (if (or (eqv? key (quote d1)) ...) (begin expr ...) ret)
          ret = List(
            Sym("if"),
            new Pair(Sym("or"), array_to_list(_.map(clause.car.to_array(), function(d){
                return List(Sym("eqv?"),
                            tmp_sym,
                            List(Sym("quote"), d));
            }))),
            new Pair(Sym("begin"), clause.cdr),
            ret
          );
        }
      });
      return new Pair(Sym("let1"),
               new Pair(tmp_sym,
                 new Pair(key,
                   new Pair(ret, nil))));
    }
  });

  define_syntax("and", function(x){
    // (and a b c) => (if a (if b c #f) #f)
    //todo: check improper list
    if(x.cdr == nil) return true;

    var objs = x.cdr.to_array();
    var i = objs.length-1;
    var t = objs[i];
    for(i=i-1; i>=0; i--)
      t = List(Sym("if"), objs[i], t, false);

    return t;
  })

  define_syntax("or", function(x){
    // (or a b c) => (if a a (if b b (if c c #f)))
    //todo: check improper list

    var objs = x.cdr.to_array()
    var f = false;
    for(var i=objs.length-1; i>=0; i--)
      f = List(Sym("if"), objs[i], objs[i], f);

    return f;
  })

  //            11.4.6  Binding constructs
  define_syntax("let", function(x){
    //(let ((a 1) (b 2)) (print a) (+ a b))
    //=> ((lambda (a b) (print a) (+ a b)) 1 2)
    var name = null;
    if (x.cdr.car instanceof Symbol) {
      name = x.cdr.car;
      x = x.cdr;
    }
    var binds = x.cdr.car, body = x.cdr.cdr;
    
    if((!(binds instanceof Pair)) && binds != BiwaScheme.nil){
      throw new Error("let: need a pair for bindings: got "+to_write(binds));
    }

    var vars = nil, vals = nil;
    for(var p=binds; p instanceof Pair; p=p.cdr){
      vars = new Pair(p.car.car, vars);
      vals = new Pair(p.car.cdr.car, vals);
    }

    var lambda = null;
    if (name) {
      // (let loop ((a 1) (b 2)) body ..)
      //=> (letrec ((loop (lambda (a b) body ..))) (loop 1 2))
      vars = array_to_list(vars.to_array().reverse());
      vals = array_to_list(vals.to_array().reverse());

      var body_lambda = new Pair(Sym("lambda"), new Pair(vars, body));
      var init_call = new Pair(name, vals);

      lambda = List(Sym("letrec"),
                    new Pair(List(name, body_lambda), nil),
                    init_call);
    }
    else {
      lambda = new Pair(new Pair(Sym("lambda"), 
                                 new Pair(vars, body)), 
                        vals);
    }
    return lambda;
  })

  define_syntax("let*", function(x){
    //(let* ((a 1) (b a)) (print a) (+ a b))
    //-> (let ((a 1)) 
    //     (let ((b a)) (print a) (+ a b)))
    var binds = x.cdr.car, body = x.cdr.cdr;
    
    if(!(binds instanceof Pair)) 
      throw new Error("let*: need a pair for bindings: got "+to_write(binds));

    var ret = null;
    _.each(binds.to_array().reverse(), function(bind){
      ret = new Pair(Sym("let"), 
               new Pair(new Pair(bind, nil),
                 ret == null ? body : new Pair(ret, nil)));
    })
    return ret;
  })

  var expand_letrec_star = function(x){
    var binds = x.cdr.car, body = x.cdr.cdr;
    
    if(!(binds instanceof Pair)) 
      throw new Error("letrec*: need a pair for bindings: got "+to_write(binds));

    var ret = body;
    _.each(binds.to_array().reverse(), function(bind){
      ret = new Pair(new Pair(Sym("set!"), bind),
              ret);
    })
    var letbody = nil;
    _.each(binds.to_array().reverse(), function(bind){
      letbody = new Pair(new Pair(bind.car, 
                           new Pair(BiwaScheme.undef, nil)),
                  letbody);
    })
    return new Pair(Sym("let"),
             new Pair(letbody,
               ret));
  }
  define_syntax("letrec", expand_letrec_star);
  define_syntax("letrec*", expand_letrec_star);

  define_syntax("let-values", function(x) {
    // (let-values (((a b) (values 1 2))
    //               ((c d . e) (values 3 4 a)))
    //              (print a b c d e))
    // =>
    // (let ((#<gensym1> (lambda () (values 1 2)))
    //       (#<gensym2> (lambda () (values 3 4 a))))
    //   (let*-values (((a b) #<gensym1>)
    //                 ((c d . e) #<gensym2>))
    //                 (print a b c d e)))
      var mv_bindings = x.cdr.car;
      var body = x.cdr.cdr;
      var ret = null;
      
      var let_bindings = nil;
      var let_star_values_bindings = nil;
      _.each(mv_bindings.to_array().reverse(), function (item) {
	  var init = item.cdr.car;
	  var tmpsym = BiwaScheme.gensym()
	  var binding = new Pair(tmpsym, 
				 new Pair(
					  new Pair(Sym("lambda"), new Pair(nil, 
									   new Pair(init, nil))),
					  nil));
	  let_bindings = new Pair(binding, let_bindings);
	  
	  var formals = item.car;
	  let_star_values_bindings = new Pair(new Pair (formals, new Pair(new Pair(tmpsym, nil), nil)),
					      let_star_values_bindings);
      });

      var let_star_values = new Pair(Sym("let*-values"),
				     new Pair(let_star_values_bindings,
					      body));
      ret = new Pair(Sym("let"), 
		     new Pair(let_bindings,
			      new Pair (let_star_values, nil)));
      return ret;
      
  });

  //let*-values
  define_syntax("let*-values", function(x){
    // (let*-values (((a b) (values 1 2))
    //               ((c d . e) (values 3 4 a)))
    //   (print a b c d e))
    // -> (call-with-values
    //      (lambda () (values 1 2))
    //      (lambda (a b)
    //        (call-with-values
    //          (lambda () (values 3 4 a))
    //          (lambda (c d . e) 
    //            (print a b c d e)))))
    var mv_bindings = x.cdr.car;
    var body = x.cdr.cdr;

    var ret = null;

    _.each(mv_bindings.to_array().reverse(), function(item){
      var formals = item.car, init = item.cdr.car;
      ret = new Pair(Sym("call-with-values"),
              new Pair(new Pair(Sym("lambda"),
                         new Pair(nil,
                           new Pair(init, nil))),
                new Pair(new Pair(Sym("lambda"),
                           new Pair(formals,
                             (ret == null ? body
                                          : new Pair(ret, nil)))), nil)));
    });
    return ret;
  });
  //            11.4.7  Sequencing
  //(begin)

  //        
  //        11.5  Equivalence predicates
  //
  BiwaScheme.eq = function(a, b){
    return a === b;
  };
   // TODO: Records (etc.)
  BiwaScheme.eqv = function(a, b){
    return a == b && (typeof(a) == typeof(b));
  };
  
  define_libfunc("eqv?", 2, 2, function(ar){
    return BiwaScheme.eqv(ar[0], ar[1]);
  })
  define_libfunc("eq?", 2, 2, function(ar){
    return BiwaScheme.eq(ar[0], ar[1]);
  })
  define_libfunc("equal?", 2, 2, function(ar){
    //TODO: must terminate for cyclic objects
    return to_write(ar[0]) == to_write(ar[1]);
  })

  //
  //        11.6  Procedure predicate
  //
  //"procedure?", 1, 1
  define_libfunc("procedure?", 1, 1, function(ar){
    return ((ar[0] instanceof Array) && (ar[0].closure_p === true)
	    || (typeof ar[0] == "function"));
  })

  //
  //        11.7  Arithmetic
  //

  //            11.7.1  Propagation of exactness and inexactness
  //            11.7.2  Representability of infinities and NaNs
  //            11.7.3  Semantics of common operations
  //                11.7.3.1  Integer division
  //                11.7.3.2  Transcendental functions
  //(no functions are introduced by above sections)

  //
  //            11.7.4  Numerical operations
  //
  
  //                11.7.4.1  Numerical type predicates
  define_libfunc("number?", 1, 1, function(ar){
    return (typeof(ar[0]) == 'number') ||
           (ar[0] instanceof Complex)  ||
           (ar[0] instanceof Rational);
  });
  define_libfunc("complex?", 1, 1, function(ar){
    return (ar[0] instanceof Complex);
  });
  define_libfunc("real?", 1, 1, function(ar){
    return (typeof(ar[0]) == 'number');
  });
  define_libfunc("rational?", 1, 1, function(ar){
    return (ar[0] instanceof Rational);
  });
  define_libfunc("integer?", 1, 1, function(ar){
    return typeof(ar[0]) == 'number'  && 
           ar[0] == Math.round(ar[0]) &&
           ar[0] != Infinity          &&
           ar[0] != -Infinity;
  });

//(real-valued? obj)    procedure 
//(rational-valued? obj)    procedure 
//(integer-valued? obj)    procedure 
//
//(exact? z)    procedure 
//(inexact? z)    procedure

  //                11.7.4.2  Generic conversions
  //
//(inexact z)    procedure 
//(exact z)    procedure
//
  //                11.7.4.3  Arithmetic operations

  //inf & nan: ok (for this section)
  define_libfunc("=", 2, null, function(ar){
    var v = ar[0];
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(ar[i] != v) return false;
    }
    return true;
  });
  define_libfunc("<", 2, null, function(ar){
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(!(ar[i-1] < ar[i])) return false;
    }
    return true;
  });
  define_libfunc(">", 2, null, function(ar){
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(!(ar[i-1] > ar[i])) return false;
    }
    return true;
  });
  define_libfunc("<=", 2, null, function(ar){
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(!(ar[i-1] <= ar[i])) return false;
    }
    return true;
  });
  define_libfunc(">=", 2, null, function(ar){
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(!(ar[i-1] >= ar[i])) return false;
    }
    return true;
  });

  define_libfunc("zero?", 1, 1, function(ar){
    assert_number(ar[0]);
    return ar[0] === 0; 
  });
  define_libfunc("positive?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] > 0);
  });
  define_libfunc("negative?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] < 0);
  });
  define_libfunc("odd?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] % 2 == 1) || (ar[0] % 2 == -1);
  })
  define_libfunc("even?", 1, 1, function(ar){
    assert_number(ar[0]);
    return ar[0] % 2 == 0;
  })
  define_libfunc("finite?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] != Infinity) && (ar[0] != -Infinity) && !isNaN(ar[0]);
  })
  define_libfunc("infinite?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] == Infinity) || (ar[0] == -Infinity);
  })
  define_libfunc("nan?", 1, 1, function(ar){
    assert_number(ar[0]);
    return isNaN(ar[0]);
  })
  define_libfunc("max", 2, null, function(ar){
    for(var i=0; i<ar.length; i++)
      assert_number(ar[i]);

    return Math.max.apply(null, ar)
  });
  define_libfunc("min", 2, null, function(ar){
    for(var i=0; i<ar.length; i++)
      assert_number(ar[i]);

    return Math.min.apply(null, ar);
  });

  define_libfunc("+", 0,null, function(ar){
    var n = 0;
    for(var i=0; i<ar.length; i++){
      assert_number(ar[i]);
      n+=ar[i];
    }
    return n;
  });
  define_libfunc("*", 0,null, function(ar){
    var n = 1;
    for(var i=0; i<ar.length; i++){
      assert_number(ar[i]);
      n*=ar[i];
    }
    return n;
  });
  define_libfunc("-", 1,null, function(ar){
    var len = ar.length;
    assert_number(ar[0]);

    if(len == 1)
      return -ar[0];
    else{
      var n = ar[0];
      for(var i=1; i<len; i++){
        assert_number(ar[i]);
        n-=ar[i];
      }
      return n;
    }
  });
  //for r6rs specification, (/ 0 0) or (/ 3 0) raises '&assertion exception'
  define_libfunc("/", 1,null, function(ar){
    var len = ar.length;
    assert_number(ar[0]);

    if(len == 1)
      return 1/ar[0];
    else{
      var n = ar[0];
      for(var i=1; i<len; i++){
        assert_number(ar[i]);
        n/=ar[i];
      }
      return n;
    }
  });

  define_libfunc("abs", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.abs(ar[0]);
  });

  var div = function(n, m){
    return Math.floor(n / m);
  }
  var mod = function(n, m){
    return n - Math.floor(n / m) * m;
  }
  var div0 = function(n, m){
    return (n > 0) ? Math.floor(n / m) : Math.ceil(n / m);
  }
  var mod0 = function(n, m){
    return (n > 0) ? n - Math.floor(n / m) * m 
                   : n - Math.ceil(n / m) * m;
  }
  define_libfunc("div0-and-mod0", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return new Values([div(ar[0], ar[1]), mod(ar[0], ar[1])]);
  })
  define_libfunc("div", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return div(ar[0], ar[1]);
  })
  define_libfunc("mod", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return mod(ar[0], ar[1]);
  })
  define_libfunc("div0-and-mod0", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return new Values([div0(ar[0], ar[1]), mod0(ar[0], ar[1])]);
  })
  define_libfunc("div0", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return div0(ar[0], ar[1]);
  })
  define_libfunc("mod0", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return mod0(ar[0], ar[1]);
  })

//(gcd n1 ...)    procedure 
//(lcm n1 ...)    procedure 

  define_libfunc("numerator", 1, 1, function(ar){
    assert_number(ar[0]);
    if(ar[0] instanceof Rational)
      return ar[0].numerator;
    else
      throw new Bug("todo");
  })
  define_libfunc("denominator", 1, 1, function(ar){
    assert_number(ar[0]);
    if(ar[0] instanceof Rational)
      return ar[0].denominator;
    else
      throw new Bug("todo");
  })
  define_libfunc("floor", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.floor(ar[0]);
  })
  define_libfunc("ceiling", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.ceil(ar[0]);
  })
  define_libfunc("truncate", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] < 0) ? Math.ceil(ar[0]) : Math.floor(ar[0]);
  })
  define_libfunc("round", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.round(ar[0]);
  })

//(rationalize x1 x2)    procedure 

  define_libfunc("exp", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.exp(ar[0]);
  })
  define_libfunc("log", 1, 2, function(ar){
    var num = ar[0], base = ar[1];
    assert_number(num);

    if(base){ // log b num == log e num / log e b
      assert_number(base);
      return Math.log(num) / Math.log(b)
    }
    else
      return Math.log(num);
  })
  define_libfunc("sin", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.sin(ar[0]);
  })
  define_libfunc("cos", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.cos(ar[0]);
  })
  define_libfunc("tan", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.tan(ar[0]);
  })
  define_libfunc("asin", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.asin(ar[0]);
  })
  define_libfunc("acos", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.acos(ar[0]);
  })
  define_libfunc("atan", 1, 2, function(ar){
    assert_number(ar[0]);
    if(ar[1]){
      assert_number(ar[1]);
      return Math.atan2(ar[0], ar[1]);
    }
    else
      return Math.atan(ar[0]);
  })
  define_libfunc("sqrt", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.sqrt(ar[0]);
  })
  define_libfunc("exact-integer-sqrt", 1, 1, function(ar){
    assert_number(ar[0]);
    var sqrt_f = Math.sqrt(ar[0]);
    var sqrt_i = sqrt_f - (sqrt_f % 1);
    var rest   = ar[0] - sqrt_i * sqrt_i;

    return new Values([sqrt_i, rest]);
  })
  define_libfunc("expt", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return Math.pow(ar[0], ar[1]);
  })
  define_libfunc("make-rectangular", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return new Complex(ar[0], ar[1]);
  })
  define_libfunc("make-polar", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return Complex.from_polar(ar[0], ar[1]);
  })
  define_libfunc("real-part", 1, 1, function(ar){
    assert_number(ar[0]);
    return Complex.assure(ar[0]).real;
  })
  define_libfunc("imag-part", 1, 1, function(ar){
    assert_number(ar[0]);
    return Complex.assure(ar[0]).imag;
  })
  define_libfunc("magnitude", 1, 1, function(ar){
    assert_number(ar[0]);
    return Complex.assure(ar[0]).magnitude();
  })
  define_libfunc("angle", 1, 1, function(ar){
    assert_number(ar[0]);
    return Complex.assure(ar[0]).angle();
  })

  //
  //                11.7.4.4  Numerical Input and Output
  //
  define_libfunc("number->string", 1, 3, function(ar){
    var z = ar[0], radix = ar[1], precision = ar[2];
    if(precision)
      throw new Bug("number->string: precision is not yet implemented");
    
    radix = radix || 10;  //TODO: check radix is 2, 8, 10, or 16.
    return z.toString(radix);
  })
  define_libfunc("string->number", 1, 3, function(ar){
    var s = ar[0], radix = ar[1] || 10;
    switch(s){
      case "+inf.0": return Infinity;
      case "-inf.0": return -Infinity;
      case "+nan.0": return NaN;
      default:       if(s.match(/[eE.]/))
                       return parseFloat(s);
                     else
                       return parseInt(s, radix);

    }
  })

  //
  //        11.8  Booleans
  //

  define_libfunc("not", 1, 1, function(ar){
    return (ar[0] === false) ? true : false;
  });
  define_libfunc("boolean?", 1, 1, function(ar){
    return (ar[0] === false || ar[0] === true) ? true : false;
  });
  define_libfunc("boolean=?", 2, null, function(ar){
    var len = ar.length;
    for(var i=1; i<len; i++){
      if(ar[i] != ar[0]) return false;
    }
    return true;
  });

  //        11.9  Pairs and lists

  define_libfunc("pair?", 1, 1, function(ar){
    return (ar[0] instanceof Pair) ? true : false;
  });
  define_libfunc("cons", 2, 2, function(ar){
    return new Pair(ar[0], ar[1]);
  });
  define_libfunc("car", 1, 1, function(ar){
    //should raise &assertion for '()...
    if(!(ar[0] instanceof Pair)) throw new Error("Attempt to apply car on " + ar[0]);
    return ar[0].car;
  });
  define_libfunc("cdr", 1, 1, function(ar){
    //should raise &assertion for '()...
    if(!(ar[0] instanceof Pair)) throw new Error("Attempt to apply cdr on " + ar[0]);
    return ar[0].cdr;
  });
  define_libfunc("set-car!", 2, 2, function(ar){
    if(!(ar[0] instanceof Pair)) throw new Error("Attempt to apply set-car! on " + ar[0]);
    ar[0].car = ar[1];
    return BiwaScheme.undef;
  });
  define_libfunc("set-cdr!", 2, 2, function(ar){
    if(!(ar[0] instanceof Pair)) throw new Error("Attempt to apply set-cdr! on " + ar[0]);
    ar[0].cdr = ar[1];
    return BiwaScheme.undef;
  });

  // cadr, caddr, cadddr, etc.
  (function(){
    // To traverse into pair and raise error
    var get = function(funcname, spec, obj){
      var ret = obj;
      _.each(spec, function(is_cdr){
        if(ret instanceof Pair){
          ret = (is_cdr ? ret.cdr : ret.car);
        }
        else{
          throw new Error(funcname+": attempt to get "+(is_cdr ? "cdr" : "car")+" of "+ret);
        }
      });
      return ret;
    };
    define_libfunc("caar", 1, 1, function(ar){ return get("caar", [0, 0], ar[0]); });
    define_libfunc("cadr", 1, 1, function(ar){ return get("cadr", [1, 0], ar[0]); });
    define_libfunc("cdar", 1, 1, function(ar){ return get("cadr", [0, 1], ar[0]); });
    define_libfunc("cddr", 1, 1, function(ar){ return get("cadr", [1, 1], ar[0]); });

    define_libfunc("caaar", 1, 1, function(ar){ return get("caaar", [0, 0, 0], ar[0]); });
    define_libfunc("caadr", 1, 1, function(ar){ return get("caadr", [1, 0, 0], ar[0]); });
    define_libfunc("cadar", 1, 1, function(ar){ return get("cadar", [0, 1, 0], ar[0]); });
    define_libfunc("caddr", 1, 1, function(ar){ return get("caddr", [1, 1, 0], ar[0]); });
    define_libfunc("cdaar", 1, 1, function(ar){ return get("cdaar", [0, 0, 1], ar[0]); });
    define_libfunc("cdadr", 1, 1, function(ar){ return get("cdadr", [1, 0, 1], ar[0]); });
    define_libfunc("cddar", 1, 1, function(ar){ return get("cddar", [0, 1, 1], ar[0]); });
    define_libfunc("cdddr", 1, 1, function(ar){ return get("cdddr", [1, 1, 1], ar[0]); });

    define_libfunc("caaaar", 1, 1, function(ar){ return get("caaaar", [0, 0, 0, 0], ar[0]); });
    define_libfunc("caaadr", 1, 1, function(ar){ return get("caaadr", [1, 0, 0, 0], ar[0]); });
    define_libfunc("caadar", 1, 1, function(ar){ return get("caadar", [0, 1, 0, 0], ar[0]); });
    define_libfunc("caaddr", 1, 1, function(ar){ return get("caaddr", [1, 1, 0, 0], ar[0]); });
    define_libfunc("cadaar", 1, 1, function(ar){ return get("cadaar", [0, 0, 1, 0], ar[0]); });
    define_libfunc("cadadr", 1, 1, function(ar){ return get("cadadr", [1, 0, 1, 0], ar[0]); });
    define_libfunc("caddar", 1, 1, function(ar){ return get("caddar", [0, 1, 1, 0], ar[0]); });
    define_libfunc("cadddr", 1, 1, function(ar){ return get("cadddr", [1, 1, 1, 0], ar[0]); });
    define_libfunc("cdaaar", 1, 1, function(ar){ return get("cdaaar", [0, 0, 0, 1], ar[0]); });
    define_libfunc("cdaadr", 1, 1, function(ar){ return get("cdaadr", [1, 0, 0, 1], ar[0]); });
    define_libfunc("cdadar", 1, 1, function(ar){ return get("cdadar", [0, 1, 0, 1], ar[0]); });
    define_libfunc("cdaddr", 1, 1, function(ar){ return get("cdaddr", [1, 1, 0, 1], ar[0]); });
    define_libfunc("cddaar", 1, 1, function(ar){ return get("cddaar", [0, 0, 1, 1], ar[0]); });
    define_libfunc("cddadr", 1, 1, function(ar){ return get("cddadr", [1, 0, 1, 1], ar[0]); });
    define_libfunc("cdddar", 1, 1, function(ar){ return get("cdddar", [0, 1, 1, 1], ar[0]); });
    define_libfunc("cddddr", 1, 1, function(ar){ return get("cddddr", [1, 1, 1, 1], ar[0]); });
  })();

  define_libfunc("null?", 1, 1, function(ar){
    return (ar[0] === nil);
  });
  define_libfunc("list?", 1, 1, function(ar){
    var contents = [];
    for(var o=ar[0]; o != nil; o=o.cdr){
      if(o == nil) return true;
      if(!(o instanceof Pair)) return false;
      if(_.detect(contents, function(item){ return item === o.car; }))
        return false; //cyclic
      contents.push(o.car);
    }
    return true;
  });
  define_libfunc("list", 0, null, function(ar){
    var l = nil;
    for(var i=ar.length-1; i>=0; i--)
      l = new Pair(ar[i], l);
    return l;
  });
  define_libfunc("length", 1, 1, function(ar){
    assert_list(ar[0]);
    var n = 0;
    for(var o=ar[0]; o!=nil; o=o.cdr)
      n++;
    return n;
  });
  define_libfunc("append", 2, null, function(ar){
    var k = ar.length
    var ret = ar[--k];
    while(k--){
      _.each(ar[k].to_array().reverse(), function(item){
        ret = new Pair(item, ret);
      });
    }
    return ret;
  });
  define_libfunc("reverse", 1, 1, function(ar){
    assert_pair(ar[0]);

    var l = nil;
    for(var o=ar[0]; o!=nil; o=o.cdr)
      l = new Pair(o.car, l);
    return l;
  });
  define_libfunc("list-tail", 2, 2, function(ar){
    assert_pair(ar[0]);
    assert_integer(ar[1]);
    if(ar[1] < 0)
      throw new Error("list-tail: index out of range ("+ar[1]+")");

    var o = ar[0];
    for(var i=0; i<ar[1]; i++){
      if(!(o instanceof Pair)) throw new Error("list-tail: the list is shorter than " + ar[1]);
      o = o.cdr;
    }
    return o;
  });
  define_libfunc("list-ref", 2, 2, function(ar){
    assert_pair(ar[0]);
    assert_integer(ar[1]);
    if(ar[1] < 0)
      throw new Error("list-tail: index out of range ("+ar[1]+")");

    var o = ar[0];
    for(var i=0; i<ar[1]; i++){
      if(!(o instanceof Pair)) throw new Error("list-ref: the list is shorter than " + ar[1]);
      o = o.cdr;
    }
    return o.car;
  });
  define_libfunc("map", 2, null, function(ar){
    var proc = ar.shift(), lists = ar;
    _.each(lists, assert_list);

    var a = [];
    return Call.multi_foreach(lists, {
      // Called for each element
      // input: the element (or the elements, if more than one list is given)
      // output: a Call request of proc and args
      call: function(xs){ 
        return new Call(proc, _.map(xs, function(x){ return x.car }));
      },

      // Called when each Call request is finished
      // input: the result of Call request,
      //   the element(s) of the Call request (which is not used here)
      // output: `undefined' to continue,
      //   some value to terminate (the value will be the result)
      result: function(res){ a.push(res); },

      // Called when reached to the end of the list(s)
      // input: none
      // output: the resultant value 
      finish: function(){ return array_to_list(a); }
    })
  })
  define_libfunc("for-each", 2, null, function(ar){
    var proc = ar.shift(), lists = ar;
    _.each(lists, assert_list);

    return Call.multi_foreach(lists, {
      call: function(xs){ 
        return new Call(proc, _.map(xs, function(x){ return x.car }));
      },
      finish: function(){ return BiwaScheme.undef; }
    })
  })

  //        11.10  Symbols

  define_libfunc("symbol?", 1, 1, function(ar){
    return (ar[0] instanceof Symbol) ? true : false;
  });
  define_libfunc("symbol->string", 1, 1, function(ar){
    assert_symbol(ar[0]);
    return ar[0].name;
  });
  define_libfunc("symbol=?", 2, null, function(ar){
    assert_symbol(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_symbol(ar[i]);
      if(ar[i] != ar[0]) return false;
    }
    return true;
  });
  define_libfunc("string->symbol", 1, 1, function(ar){
    assert_string(ar[0]);
    return Sym(ar[0]);
  });

  //
  //        11.11  Characters
  //
  define_libfunc('char?', 1, 1, function(ar){
    return (ar[0] instanceof Char);
  });
  define_libfunc('char->integer', 1, 1, function(ar){
    assert_char(ar[0]);
    return ar[0].value.charCodeAt(0);
  })
  define_libfunc('integer->char', 1, 1, function(ar){
    assert_integer(ar[0]);
    return Char.get(String.fromCharCode(ar[0]));
  })

  var make_char_compare_func = function(test){
    return function(ar){
      assert_char(ar[0]);
      for(var i=1; i<ar.length; i++){
        assert_char(ar[i]);
        if(!test(ar[i-1].value, ar[i].value))
          return false;
      }
      return true;
    }
  }
  define_libfunc('char=?', 2, null, 
    make_char_compare_func(function(a, b){ return a == b }))
  define_libfunc('char<?', 2, null, 
    make_char_compare_func(function(a, b){ return a < b }))
  define_libfunc('char>?', 2, null, 
    make_char_compare_func(function(a, b){ return a > b }))
  define_libfunc('char<=?', 2, null, 
    make_char_compare_func(function(a, b){ return a <= b }))
  define_libfunc('char>=?', 2, null, 
    make_char_compare_func(function(a, b){ return a >= b }))

  //
  //        11.12  Strings
  //
  define_libfunc("string?", 1, 1, function(ar){
    return (typeof(ar[0]) == "string"); 
  })
  define_libfunc("make-string", 1, 2, function(ar){
    assert_integer(ar[0]);
    var c = " ";
    if(ar[1]){
      assert_char(ar[1]);
      c = ar[1].value;
    }
    var out = "";
    _.times(ar[0], function() { out += c; });
    return out;
  })
  define_libfunc("string", 1, null, function(ar){
    for(var i=0; i<ar.length; i++)
      assert_char(ar[i]);
    return _.map(ar, function(c){ return c.value }).join("");
  })
  define_libfunc("string-length", 1, 1, function(ar){
    assert_string(ar[0]);
    return ar[0].length;
  })
  define_libfunc("string-ref", 2, 2, function(ar){
    assert_string(ar[0]);
    assert_between(ar[1], 0, ar[0].length-1);
    return Char.get(ar[0].charAt([ar[1]]));
  })
  define_libfunc("string=?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(ar[0] != ar[i]) return false;
    }
    return true;
  })
  define_libfunc("string<?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(!(ar[i-1] < ar[i])) return false;
    }
    return true;
  })
  define_libfunc("string>?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(!(ar[i-1] > ar[i])) return false;
    }
    return true;
  })
  define_libfunc("string<=?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(!(ar[i-1] <= ar[i])) return false;
    }
    return true;
  })
  define_libfunc("string>=?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(!(ar[i-1] >= ar[i])) return false;
    }
    return true;
  })

  define_libfunc("substring", 3, 3, function(ar){
    assert_string(ar[0]);
    assert_integer(ar[1]);
    assert_integer(ar[2]);

    if(ar[1] < 0) throw new Error("substring: start too small: "+ar[1]);
    if(ar[2] < 0) throw new Error("substring: end too small: "+ar[2]);
    if(ar[0].length+1 <= ar[1]) throw new Error("substring: start too big: "+ar[1]);
    if(ar[0].length+1 <= ar[2]) throw new Error("substring: end too big: "+ar[2]);
    if(!(ar[1] <= ar[2])) throw new Error("substring: not start <= end: "+ar[1]+", "+ar[2]);
    
    return ar[0].substring(ar[1], ar[2]);
  })

  define_libfunc("string-append", 0, null, function(ar){
    for(var i=0; i<ar.length; i++)
      assert_string(ar[i]);
    
    return ar.join("");
  })
  define_libfunc("string->list", 1, 1, function(ar){
    assert_string(ar[0]);
    return array_to_list(_.map(ar[0].split(""), function(s){ return Char.get(s[0]); }));
  })
  define_libfunc("list->string", 1, 1, function(ar){
    assert_list(ar[0]);
    return _.map(ar[0].to_array(), function(c){ return c.value; }).join("");
  })
  define_libfunc("string-for-each", 2, null, function(ar){
    var proc = ar.shift(), strs = ar;
    _.each(strs, assert_string);
    
    return Call.multi_foreach(strs, {
      call: function(chars){ return new Call(proc, chars); },
      finish: function(){ return BiwaScheme.undef; }
    })
  })
  define_libfunc("string-copy", 1, 1, function(ar){
    // note: this is useless, because javascript strings are immutable
    assert_string(ar[0]);
    return ar[0];
  })


  //
  //        11.13  Vectors
  //
  define_libfunc("vector?", 1, 1, function(ar){
    return (ar[0] instanceof Array) && (ar[0].closure_p !== true)
  })
  define_libfunc("make-vector", 1, 2, function(ar){
    assert_integer(ar[0]);
    var vec = new Array(ar[0]);

    if(ar.length == 2){
      for(var i=0; i<ar[0]; i++)
        vec[i] = ar[1];
    }
    return vec;
  })
  define_libfunc("vector", 1, null, function(ar){
    return ar;
  })
  define_libfunc("vector-length", 1, 1, function(ar){
    assert_vector(ar[0]);
    return ar[0].length;
  })
  define_libfunc("vector-ref", 2, 2, function(ar){
    assert_vector(ar[0]);
    assert_integer(ar[1]);
    assert_between(ar[1], 0, ar[0].length-1);

    return ar[0][ar[1]];
  })
  define_libfunc("vector-set!", 3, 3, function(ar){
    assert_vector(ar[0]);
    assert_integer(ar[1]);

    ar[0][ar[1]] = ar[2];
    return BiwaScheme.undef;
  })
  define_libfunc("vector->list", 1, 1, function(ar){
    assert_vector(ar[0]);
    return array_to_list(ar[0]);
  })
  define_libfunc("list->vector", 1, 1, function(ar){
    assert_list(ar[0]);
    return ar[0].to_array();
  })
  define_libfunc("vector-fill!", 2, 2, function(ar){
    assert_vector(ar[0]);
    var vec = ar[0], obj = ar[1];

    for(var i=0; i<vec.length; i++)
      vec[i] = obj;
    return vec;
  })
  define_libfunc("vector-map", 2, null, function(ar){
    var proc = ar.shift(), vecs = ar;
    _.each(vecs, assert_vector);

    var a = [];
    return Call.multi_foreach(vecs, {
      call: function(objs){ return new Call(proc, objs); },
      result: function(res){ a.push(res); },
      finish: function(){ return a; }
    })
  })
  define_libfunc("vector-for-each", 2, null, function(ar){
    var proc = ar.shift(), vecs = ar;
    _.each(vecs, assert_vector);

    return Call.multi_foreach(vecs, {
      call: function(objs){ return new Call(proc, objs); },
      finish: function(){ return BiwaScheme.undef; }
    })
  })

  //
  //        11.14  Errors and violations
  //
//(error who message irritant1 ...)    procedure 
//(assertion-violation who message irritant1 ...)    procedure 
//(assert <expression>)    syntax 
  
  //
  //        11.15  Control features
  //
  define_libfunc("apply", 2, null, function(ar){
    var proc = ar.shift(), rest_args = ar.pop(), args = ar;
    args = args.concat(rest_args.to_array());

    return new Call(proc, args);
  })
  define_syntax("call-with-current-continuation", function(x){
    return new Pair(Sym("call/cc"),
             x.cdr);
  })
  define_libfunc("values", 0, null, function(ar){
    return new Values(ar);
  })
  define_libfunc("call-with-values", 2, 2, function(ar){
    var producer = ar[0], consumer = ar[1];
    return new Call(producer, [], function(ar){
      var values = ar[0];
      if(!(values instanceof Values))
        throw new Error("values expected, but got "+to_write(values));

      return new Call(consumer, values.content);
    })
  })

  //
  //dynamic-wind
  
  //        11.16  Iteration
  //named let
  
  //        11.17  Quasiquotation
  //quasiquote
  var expand_qq = function(f, lv){
    if(f instanceof Symbol || f === nil){
      return List(Sym("quote"), f);
    }
    else if(f instanceof Pair){
      var car = f.car;
      if(car instanceof Pair && car.car === Sym("unquote-splicing")){
        var lv = lv-1;
        if(lv == 0)
          return List(Sym("append"),
                      f.car.cdr.car,
                      expand_qq(f.cdr, lv+1));
        else
          return List(Sym("cons"),
                      List(Sym("list"), Sym("unquote-splicing"), expand_qq(f.car.cdr.car, lv)),
                      expand_qq(f.cdr, lv+1));
      }
      else if(car === Sym("unquote")){
        var lv = lv-1;
        if(lv == 0)
          return f.cdr.car;
        else
          return List(Sym("list"),
                      List(Sym("quote"), Sym("unquote")),
                      expand_qq(f.cdr.car, lv));
      }
      else if(car === Sym("quasiquote"))
        return List(Sym("list"),
                    Sym("quasiquote"),
                    expand_qq(f.cdr.car, lv+1));
      else
        return List(Sym("cons"),
                    expand_qq(f.car, lv),
                    expand_qq(f.cdr, lv));
    }
    else if(f instanceof Array){
      throw new Bug("vector quasiquotation is not implemented yet");
    }
//      // `#(1 2 (unquote f))
//      // (vector 1 2 f)
//      // `#(1 2 (unquote-splicing f) 3)
//      // (vector-append
//      //   (vector 1 2)
//      //   f
//      //   (vector 3))
//      // `#(1 2 `#(3 ,,f) 4)
//      // (vector 1 2 `#(3 ,g) 4)
//      var len = f.length;
//      if(len == 0) return f;
//
//      var vecs = [[]];
//      for(var i=0; i<len; i++){
//        if(f[i] instanceof Pair){
//          if(f[i].car === Sym("unquote")){
//            var lv = lv - 1;
//            if(lv == 0)
//              vecs.last().push(f[i]);
//            else
//              vecs.push()
//          }
//      }
//
//      var car = f[0];
//      if(car === Sym("unquote")){
//        var lv = lv - 1;
//        if(lv == 0)
//          return f.cdr.car;
//        else
//          return List(Sym("vector"),
//                      List(Sym("quote"), Sym("unquote")),
//                      expand_qq(f.cdr.car, lv));
//      }
//      else{
////        return [ Sym("vector"),
////                 expand_qq(
//      }
//    }
//  }
    else
      return f;
  }
  define_syntax("quasiquote", function(x){
    return expand_qq(x.cdr.car, 1);
  })
  //unquote
  define_syntax("unquote", function(x){
    throw new Error("unquote(,) must be inside quasiquote(`)");
  })
  //unquote-splicing
  define_syntax("unquote-splicing", function(x){
    throw new Error("unquote-splicing(,@) must be inside quasiquote(`)");
  })
  
  //        11.18  Binding constructs for syntactic keywords
  //let-syntax
  //letrec-syntax
  
  //        11.19  Macro transformers
  //syntax-rules
  //identifier-syntax
  //
  
  //        11.20  Tail calls and tail contexts
  //(no library function introduced)


  ///
  /// R6RS Standard Libraries
  ///

  //
  // Chapter 1 Unicode
  //
//(char-upcase char)    procedure 
//(char-downcase char)    procedure 
//(char-titlecase char)    procedure 
//(char-foldcase char)    procedure 
//
//(char-ci=? char1 char2 char3 ...)    procedure 
//(char-ci<? char1 char2 char3 ...)    procedure 
//(char-ci>? char1 char2 char3 ...)    procedure 
//(char-ci<=? char1 char2 char3 ...)    procedure 
//(char-ci>=? char1 char2 char3 ...)    procedure 
//
//(char-alphabetic? char)    procedure 
//(char-numeric? char)    procedure 
//(char-whitespace? char)    procedure 
//(char-upper-case? char)    procedure 
//(char-lower-case? char)    procedure 
//(char-title-case? char)    procedure 
//
//(char-general-category char)    procedure 

  //(string-upcase string)    procedure 
  define_libfunc("string-upcase", 1, 1, function(ar){
    assert_string(ar[0]);
    return ar[0].toUpperCase();
  });
  //(string-downcase string)    procedure 
  define_libfunc("string-downcase", 1, 1, function(ar){
    assert_string(ar[0]);
    return ar[0].toLowerCase();
  });
//(string-titlecase string)    procedure 
//(string-foldcase string)    procedure

  BiwaScheme.make_string_ci_function = function(compare){
    return function(ar){
      assert_string(ar[0]);
      var str = ar[0].toUpperCase();

      for(var i=1; i<ar.length; i++){
        assert_string(ar[i]);
        if (!compare(str, ar[i].toUpperCase()))
          return false;
      }
      return true;
    }
  };
  //(string-ci=? string1 string2 string3 ...)    procedure 
  define_libfunc("string-ci=?", 2, null, 
    make_string_ci_function(function(a, b){ return a == b; }));
  //(string-ci<? string1 string2 string3 ...)    procedure 
  define_libfunc("string-ci<?", 2, null, 
    make_string_ci_function(function(a, b){ return a < b; }));
  //(string-ci>? string1 string2 string3 ...)    procedure 
  define_libfunc("string-ci>?", 2, null, 
    make_string_ci_function(function(a, b){ return a > b; }));
  //(string-ci<=? string1 string2 string3 ...)    procedure 
  define_libfunc("string-ci<=?", 2, null, 
    make_string_ci_function(function(a, b){ return a <= b; }));
  //(string-ci>=? string1 string2 string3 ...)    procedure 
  define_libfunc("string-ci>=?", 2, null, 
    make_string_ci_function(function(a, b){ return a >= b; }));

//(string-normalize-nfd string)    procedure 
//(string-normalize-nfkd string)    procedure 
//(string-normalize-nfc string)    procedure 
//(string-normalize-nfkc string)    procedure 

  //
  // Chapter 2 Bytevectors
  //

  //
  // Chapter 3 List utilities
  //
  define_libfunc("find", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_list(ls);
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ if(res) return x.car; },
      finish: function(){ return false }
    })
  })
  define_libfunc("for-all", 2, null, function(ar){
    var proc = ar.shift();
    var lists = ar;
    _.each(lists, assert_list);

    var last = true; //holds last result which proc returns
    return Call.multi_foreach(lists, {
      call: function(pairs){ 
        return new Call(proc, _.map(pairs, function(x){ return x.car }));
      },
      result: function(res, pairs){ 
        if(res === false) return false;
        last = res;
      },
      finish: function(){ return last; }
    })
  })
  define_libfunc("exists", 2, null, function(ar){
    var proc = ar.shift();
    var lists = ar;
    _.each(lists, assert_list);

    return Call.multi_foreach(lists, {
      call: function(pairs){ 
        return new Call(proc, _.map(pairs, function(x){ return x.car }));
      },
      result: function(res, pairs){ 
        if(res !== false) return res;
      },
      finish: function(){ return false; }
    })
  })
  define_libfunc("filter", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_list(ls);

    var a = [];
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ if(res) a.push(x.car); },
      finish: function(){ return array_to_list(a) }
    })
  })
//  define_scmfunc("partition+", 2, 2, 
//    "(lambda (proc ls)  \
//       (define (partition2 proc ls t f) \
//         (if (null? ls) \
//           (values (reverse t) (reverse f)) \
//           (if (proc (car ls)) \
//             (partition2 proc (cdr ls) (cons (car ls) t) f) \
//             (partition2 proc (cdr ls) t (cons (car ls) f))))) \
//       (partition2 proc ls '() '()))");

  define_libfunc("partition", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_list(ls);

    var t = [], f = [];
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ 
        if(res) t.push(x.car); 
        else    f.push(x.car); 
      },
      finish: function(){ 
        return new Values([array_to_list(t), array_to_list(f)]);
      }
    })
  })
  define_libfunc("fold-left", 3, null, function(ar){
    var proc = ar.shift(), accum = ar.shift(), lists = ar;
    _.each(lists, assert_list);

    return Call.multi_foreach(lists, {
      call: function(pairs){ 
        var args = _.map(pairs, function(x){ return x.car });
        args.unshift(accum);
        return new Call(proc, args);
      },
      result: function(res, pairs){ accum = res; },
      finish: function(){ return accum; }
    })
  })
  define_libfunc("fold-right", 3, null, function(ar){
    var proc = ar.shift(), accum = ar.shift();
    var lists = _.map(ar, function(ls){
      // reverse each list
      assert_list(ls);
      return array_to_list(ls.to_array().reverse());
    })

    return Call.multi_foreach(lists, {
      call: function(pairs){ 
        var args = _.map(pairs, function(x){ return x.car });
        args.push(accum);
        return new Call(proc, args);
      },
      result: function(res, pairs){ accum = res; },
      finish: function(){ return accum; }
    })
  })
  define_libfunc("remp", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_list(ls);

    var ret = [];
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ if(!res) ret.push(x.car); },
      finish: function(){ return array_to_list(ret); }
    })
  })
  var make_remover = function(key){
    return function(ar){ 
      var obj = ar[0], ls = ar[1];
      assert_list(ls);

      var ret = [];
      return Call.foreach(ls, {
        call: function(x){ 
          return new Call(TopEnv[key] || CoreEnv[key], [obj, x.car]) 
        },
        result: function(res, x){ if(!res) ret.push(x.car); },
        finish: function(){ return array_to_list(ret); }
      })
    }
  }
  define_libfunc("remove", 2, 2, make_remover("equal?"));
  define_libfunc("remv", 2, 2, make_remover("eqv?"));
  define_libfunc("remq", 2, 2, make_remover("eq?"));

  define_libfunc("memp", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_list(ls);

    var ret = [];
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ if(res) return x; },
      finish: function(){ return false; }
    })
  })
  var make_finder = function(key){
    return function(ar){ 
      var obj = ar[0], ls = ar[1];
      assert_list(ls);

      var ret = [];
      return Call.foreach(ls, {
        call: function(x){ 
          return new Call(TopEnv[key] || CoreEnv[key], [obj, x.car]) 
        },
        result: function(res, x){ if(res) return x; },
        finish: function(){ return false; }
      })
    }
  }
  define_libfunc("member", 2, 2, make_finder("equal?"));
  define_libfunc("memv", 2, 2, make_finder("eqv?"));
  define_libfunc("memq", 2, 2, make_finder("eq?"));
  
  define_libfunc("assp", 2, 2, function(ar){
    var proc = ar[0], als = ar[1];
    assert_list(als);

    var ret = [];
    return Call.foreach(als, {
      call: function(x){ 
        if(x.car.car)
          return new Call(proc, [x.car.car]);
        else
          throw new Error("ass*: pair required but got "+to_write(x.car));
      },
      result: function(res, x){ if(res) return x.car; },
      finish: function(){ return false; }
    })
  })
  var make_assoc = function(func_name, eq_func_name){
    return function(ar){ 
      var obj = ar[0], list = ar[1];
      assert_list(list);

      var ret = [];
      return Call.foreach(list, {
        call: function(ls){ 
          if(!BiwaScheme.isPair(ls.car))
            throw new Error(func_name+": pair required but got "+to_write(ls.car));
            
          var equality = (TopEnv[eq_func_name] || CoreEnv[eq_func_name]);
          return new Call(equality, [obj, ls.car.car]);
        },
        result: function(was_equal, ls){ if(was_equal) return ls.car; },
        finish: function(){ return false; }
      })
    }
  }
  define_libfunc("assoc", 2, 2, make_assoc("assoc", "equal?"));
  define_libfunc("assv", 2, 2, make_assoc("assv", "eqv?"));
  define_libfunc("assq", 2, 2, make_assoc("assq", "eq?"));

  define_libfunc("cons*", 1, null, function(ar){
    if(ar.length == 1)
      return ar[0];
    else{
      var ret = null;
      _.each(ar.reverse(), function(x){
        if(ret){
          ret = new Pair(x, ret);
        }
        else
          ret = x;
      })
      return ret;
    }
  })

  //
  // Chapter 4 Sorting
  //
  // NOTE: compare function is not supported yet.
  // it is partially implemented as list-sort/comp
  // (see extra_lib.js).
  // TODO: implement some sorting algorithm in CPS
  // so that setTimeout in compare procedures work well
  //
  //(list-sort list)
  define_libfunc("list-sort", 1, 2, function(ar){
    if(ar[1]){
      throw new Bug("list-sort: cannot take compare proc now");
    }
    assert_list(ar[0]);
    return array_to_list(ar[0].to_array().sort());
  });

  //(vector-sort proc vector)    procedure
  define_libfunc("vector-sort", 1, 2, function(ar){
    if(ar[1]){
      throw new Bug("vector-sort: cannot take compare proc now");
    }
    assert_vector(ar[0]);
      return _.clone(ar[0]).sort();
  });

  //(vector-sort! proc vector)    procedure 
  define_libfunc("vector-sort!", 1, 2, function(ar){
    if(ar[1]){
      throw new Bug("vector-sort!: cannot take compare proc now");
    }
    assert_vector(ar[0]);
    ar[0].sort();
    return BiwaScheme.undef;
  });

  //
  // Chapter 5 Control Structures
  //
  define_syntax("when", function(x){
    //(when test body ...) 
    //=> (if test (begin body ...) #<undef>)
    var test = x.cdr.car, body = x.cdr.cdr;

    return new Pair(Sym("if"), 
             new Pair(test,
               new Pair(new Pair(Sym("begin"), body),
                 new Pair(BiwaScheme.undef, nil))));
  });

  define_syntax("unless", function(x){
    //(unless test body ...) 
    //=> (if (not test) (begin body ...) #<undef>)
    var test = x.cdr.car, body = x.cdr.cdr;

    return new Pair(Sym("if"), 
             new Pair(new Pair(Sym("not"), new Pair(test, nil)),
               new Pair(new Pair(Sym("begin"), body),
                 new Pair(BiwaScheme.undef, nil))));
  });

  define_syntax("do", function(x){
    //(do ((var1 init1 step1)
    //     (var2 init2 step2) ...)
    //    (test expr1 expr2 ...)
    //  body1 body2 ...)
    //=> (let loop` ((var1 init1) (var2 init2) ...)
    //     (if test 
    //       (begin expr1 expr2 ...)
    //       (begin body1 body2 ...
    //              (loop` step1 step2 ...)))))

    // parse arguments
    if(!BiwaScheme.isPair(x.cdr))
      throw new Error("do: no variables of do");
    var varsc = x.cdr.car;
    if(!BiwaScheme.isPair(varsc))
      throw new Error("do: variables must be given as a list");
    if(!BiwaScheme.isPair(x.cdr.cdr))
      throw new Error("do: no resulting form of do");
    var resultc = x.cdr.cdr.car;
    var bodyc = x.cdr.cdr.cdr;

    // construct subforms
    var loop = BiwaScheme.gensym();

    var init_vars = array_to_list(varsc.map(function(var_def){
      var a = var_def.to_array();
      return List(a[0], a[1]);
    }));

    var test = resultc.car;
    var result_exprs = new Pair(Sym("begin"), resultc.cdr);

    var next_loop = new Pair(loop, array_to_list(varsc.map(function(var_def){
      var a = var_def.to_array();
      return a[2] || a[0];
    })));
    var body_exprs = new Pair(Sym("begin"), bodyc).concat(List(next_loop));

    // combine subforms 
    return List(Sym("let"), 
                loop,
                init_vars,
                List(Sym("if"),
                     test,
                     result_exprs,
                     body_exprs));
  });

  //(case-lambda <case-lambda clause> ...)    syntax
  define_syntax("case-lambda", function(x){
    // (case-lambda (() body0 ...)
    //              ((a) body1 ...)
    //              ((a b . cc) body2 ...)
    //              (rest bodyn ...))
    //=> (lambda args`
    //     (let1 len` (length args`)
    //       (if (= len` (length '()))
    //         ((lambda () body0 ...) args`)
    //         (if (= len` (length '(a)))
    //           ((lambda (a) body1 ...) args`)
    //           (if (>= len` (length '(a b)))
    //             ((lambda (a b . cc) body2 ...) args`)
    //             ((lambda rest bodyn ...) args`)
//    var len = BiwaScheme.gensym();
//    if(!BiwaScheme.isPair(x.cdr))
//      throw new Error("do: no variables of do");

  });

  //
  // Chapter 6 Records
  // see also: src/system/record.js
  //

  // 6.2 Records: Syntactic layer
  //eqv, eq

  //(define-record-type <name spec> <record clause>*)    syntax 
  define_syntax("define-record-type", function(x){
    // (define-record-type <name spec> <record clause>*)
    var name_spec = x.cdr.car;
    var record_clauses = x.cdr.cdr;

    // 1. parse name spec
    // <name spec>: either
    // - <record name> eg: point
    // - (<record name> <constructor name> <predicate name>) 
    //   eg: (point make-point point?)
    if(BiwaScheme.isSymbol(name_spec)){
      var record_name = name_spec;
      var constructor_name = Sym("make-"+name_spec.name);
      var predicate_name = Sym(name_spec.name+"?");
    }
    else{
      assert_list(name_spec);
      var record_name = name_spec.car;
      var constructor_name = name_spec.cdr.car;
      var predicate_name = name_spec.cdr.cdr.car;
      assert_symbol(record_name);
      assert_symbol(constructor_name);
      assert_symbol(predicate_name);
    }
    
    // 2. parse record clauses
    var sealed = false;
    var opaque = false;
    var nongenerative = false;
    var uid = false;
    var parent_name;
    var parent_rtd = false;
    var parent_cd = false;
    var protocol = false;
    var fields = [];

    // <record clause>:
    _.each(record_clauses.to_array(), function(clause){
      switch(clause.car){
        // - (fields <field spec>*)
        case Sym("fields"):
          fields = _.map(clause.cdr.to_array(), function(field_spec, idx){
            if(BiwaScheme.isSymbol(field_spec)){
              // - <field name>
              return {name: field_spec, idx: idx, mutable: false,
                      accessor_name: null, mutator_name: null};
            }
            else{
              assert_list(field_spec);
              assert_symbol(field_spec.car);
              switch(field_spec.car){
                case Sym("immutable"):
                  // - (immutable <field name>)
                  // - (immutable <field name> <accessor name>)
                  var field_name = field_spec.cdr.car;
                  assert_symbol(field_name);

                  if(BiwaScheme.isNil(field_spec.cdr.cdr))
                    return {name: field_name, idx: idx, mutable: false};
                  else
                    return {name: field_name, idx: idx, mutable: false,
                            accessor_name: field_spec.cdr.cdr.car};
                  break;

                case Sym("mutable"):
                  // - (mutable <field name>)
                  // - (mutable <field name> <accessor name> <mutator name>)
                  var field_name = field_spec.cdr.car;
                  assert_symbol(field_name);

                  if(BiwaScheme.isNil(field_spec.cdr.cdr))
                    return {name: field_name, idx: idx, mutable: true}
                  else
                    return {name: field_name, idx: idx, mutable: true,
                            accessor_name: field_spec.cdr.cdr.car,
                            mutator_name: field_spec.cdr.cdr.cdr.car};
                  break;
                default:
                  throw new Error("define-record-type: field definition "+
                              "must start with `immutable' or 'mutable'");
              }
            }
          });
          break;
        // - (parent <name>)
        case Sym("parent"):
          parent_name = clause.cdr.car;
          assert_symbol(parent_name);
          break;
        // - (protocol <expr>)
        case Sym("protocol"):
          protocol = clause.cdr.car;
          break;
        // - (sealed <bool>)
        case Sym("sealed"):
          sealed = !!clause.cdr.car;
          break;
        // - (opaque <bool>)
        case Sym("opaque"):
          opaque = !!clause.cdr.car;
          break;
        // - (nongenerative <uid>?)
        case Sym("nongenerative"):
          nongenerative = true;
          uid = clause.cdr.car;
          break;
        // - (parent-rtd <rtd> <cd>)
        case Sym("parent-rtd"):
          parent_rtd = clause.cdr.car;
          parent_cd = clause.cdr.cdr.car;
          break;
        default:
          throw new BiwaScheme.Error("define-record-type: unknown clause `"+
                                     BiwaScheme.to_write(clause.car)+"'");
      }
    });

    if(parent_name){
      parent_rtd = [Sym("record-type-descriptor"), parent_name];
      parent_cd  = [Sym("record-constructor-descriptor"), parent_name];
    }

    // 3. build the definitions
    // Note: In this implementation, rtd and cd are not bound to symbols.
    // They are referenced through record name by record-type-descriptor
    // and record-constructor-descriptor. These relation are stored in
    // the hash BiwaScheme.Record._DefinedTypes.
    var rtd = [Sym("record-type-descriptor"), record_name];
    var cd  = [Sym("record-constructor-descriptor"), record_name];

    // registration
    var rtd_fields = _.map(fields, function(field){
      return List(Sym(field.mutable ? "mutable" : "immutable"), field.name);
    });
    rtd_fields.is_vector = true; //tell List not to convert
    var rtd_def = [Sym("make-record-type-descriptor"),
                    [Sym("quote"), record_name], parent_rtd, uid,
                    sealed, opaque, rtd_fields];
    var cd_def = [Sym("make-record-constructor-descriptor"),
                    Sym("__rtd"), parent_cd, protocol];
    var registration =
      [Sym("let*"), [[Sym("__rtd"), rtd_def],
                    [Sym("__cd"), cd_def]],
        [Sym("_define-record-type"),
          [Sym("quote"), record_name], Sym("__rtd"), Sym("__cd")]];

    // accessors and mutators
    var accessor_defs = _.map(fields, function(field){
      var name = field.accessor_name ||
                   Sym(record_name.name+"-"+field.name.name);

      return [Sym("define"), name, [Sym("record-accessor"), rtd, field.idx]];
    });

    var mutator_defs = _.filter(fields, function(field){
      return field.mutable;
    });
    mutator_defs = _.map(mutator_defs, function(field){
      var name = field.mutator_name ||
                   Sym("set-"+record_name.name+"-"+field.name.name+"!");

      return [Sym("define"), name, [Sym("record-mutator"), rtd, field.idx]];
    });

    // Wrap the definitions with `begin'
    // Example:
    //   (begin
    //     (let* ((__rtd (make-record-type-descriptor 'square
    //                     (record-type-descriptor rect)
    //                     #f #f #f 
    //                     #((mutable w) (mutable h))))
    //            (__cd (make-record-constructor-descriptor __rtd
    //                    (record-constructor-descriptor rect)
    //                    (lambda (n) ...))))
    //       (_define-record-type 'square __rtd __cd))
    //
    //     (define make-square
    //       (record-constructor
    //         (record-constructor-descriptor square)))
    //     (define square?
    //       (record-predicate (record-type-descriptor square)))
    //     (define square-w
    //       (record-accessor (record-type-descriptor square) 0))
    //     (define square-h
    //       (record-accessor (record-type-descriptor square) 1))
    //     (define set-square-w!
    //       (record-mutator (record-type-descriptor square) 0))
    //     (define set-square-h!
    //       (record-mutator (record-type-descriptor square) 1)))
    //
    return deep_array_to_list(
      [Sym("begin"),
        registration,
        [Sym("define"), constructor_name, [Sym("record-constructor"), cd]],
        [Sym("define"), predicate_name, [Sym("record-predicate"), rtd]],
        ].concat(accessor_defs).
          concat(mutator_defs)
    );
  });

  define_libfunc("_define-record-type", 3, 3, function(ar){
    assert_symbol(ar[0]);
    assert_record_td(ar[1]);
    assert_record_cd(ar[2]);
    BiwaScheme.Record.define_type(ar[0].name, ar[1], ar[2]);
    return BiwaScheme.undef;
  });

  //(record-type-descriptor <record name>)    syntax 
  define_syntax("record-type-descriptor", function(x){
    return deep_array_to_list([Sym("_record-type-descriptor"), [Sym("quote"), x.cdr.car]]);
  });
  define_libfunc("_record-type-descriptor", 1, 1, function(ar){
    assert_symbol(ar[0]);
    var type = BiwaScheme.Record.get_type(ar[0].name);
    if(type)
      return type.rtd;
    else
      throw new Error("record-type-descriptor: unknown record type "+ar[0].name);
  });

  //(record-constructor-descriptor <record name>)    syntax 
  define_syntax("record-constructor-descriptor", function(x){
    return deep_array_to_list([Sym("_record-constructor-descriptor"), [Sym("quote"), x.cdr.car]]);
  });
  define_libfunc("_record-constructor-descriptor", 1, 1, function(ar){
    assert_symbol(ar[0]);
    var type = BiwaScheme.Record.get_type(ar[0].name);
    if(type)
      return type.cd;
    else
      throw new Error("record-constructor-descriptor: unknown record type "+ar[0].name);
  });

  // 6.3  Records: Procedural layer
  //(make-record-type-descriptor name    procedure
  define_libfunc("make-record-type-descriptor", 6, 6, function(ar){
    var name = ar[0], parent_rtd = ar[1], uid = ar[2],
        sealed = ar[3], opaque = ar[4], fields = ar[5];
    
    assert_symbol(name);
    if(parent_rtd) assert_record_td(parent_rtd);
    if(uid){
      assert_symbol(uid);
      var _rtd = BiwaScheme.Record.RTD.NongenerativeRecords[uid.name];
      if(_rtd){
        // the record type is already defined.
        return _rtd;
        // should check equality of other arguments..
      }
    }
    sealed = !!sealed;
    opaque = !!opaque;
    assert_vector(fields);
    for(var i=0; i<fields.length; i++){
      var list = fields[i];
      assert_symbol(list.car, "mutability");
      assert_symbol(list.cdr.car, "field name");
      fields[i] = [list.cdr.car.name, (list.car == Sym("mutable"))];
    };

    var rtd = new BiwaScheme.Record.RTD(name, parent_rtd, uid,
                                     sealed, opaque, fields);
    if(uid)
      BiwaScheme.Record.RTD.NongenerativeRecords[uid.name] = rtd;

    return rtd;
  });

  //(record-type-descriptor? obj)    procedure 
  define_libfunc("record-type-descriptor?", 1, 1, function(ar){
    return (ar[0] instanceof BiwaScheme.Record.RTD);
  });

  //(make-record-constructor-descriptor rtd    procedure 
  define_libfunc("make-record-constructor-descriptor", 3, 3, function(ar){
    var rtd = ar[0], parent_cd = ar[1], protocol = ar[2];

    assert_record_td(rtd);
    if(parent_cd) assert_record_cd(parent_cd);
    if(protocol) assert_procedure(protocol);

    return new BiwaScheme.Record.CD(rtd, parent_cd, protocol);
  });

  //(record-constructor constructor-descriptor)    procedure
  define_libfunc("record-constructor", 1, 1, function(ar){
    var cd = ar[0];
    assert_record_cd(cd);

    return cd.record_constructor();
  });

  //(record-predicate rtd)    procedure
  define_libfunc("record-predicate", 1, 1, function(ar){
    var rtd = ar[0];
    assert_record_td(rtd);

    return function(args){
      var obj = args[0];

      return (obj instanceof BiwaScheme.Record) &&
             (obj.rtd === rtd);
    };
  });

  //(record-accessor rtd k)    procedure 
  define_libfunc("record-accessor", 2, 2, function(ar){
    var rtd = ar[0], k = ar[1];
    assert_record_td(rtd);
    assert_integer(k);
    for(var _rtd = rtd.parent_rtd; _rtd; _rtd = _rtd.parent_rtd)
      k += _rtd.fields.length;

    return function(args){
      var record = args[0];
      var error_msg = rtd.name.name+"-"+rtd.field_name(k)+": "+
                      BiwaScheme.to_write(record)+
                      " is not a "+rtd.name.name;
      assert(BiwaScheme.isRecord(record), error_msg);

      var descendant = false;
      for(var _rtd = record.rtd; _rtd; _rtd = _rtd.parent_rtd){
        if(_rtd == rtd) descendant = true;
      }
      assert(descendant, error_msg);

      return record.get(k);
    };
  });

  //(record-mutator rtd k)    procedure
  define_libfunc("record-mutator", 2, 2, function(ar){
    var rtd = ar[0], k = ar[1];
    assert_record_td(rtd);
    assert_integer(k);
    for(var _rtd = rtd.parent_rtd; _rtd; _rtd = _rtd.parent_rtd)
      k += _rtd.fields.length;

    return function(args){
      var record = args[0], val = args[1];
      var func_name = rtd.field_name(k);

      assert_record(record);
      assert(record.rtd === rtd,
            func_name+": "+BiwaScheme.to_write(record)+
            " is not a "+rtd.name.name);
      assert(!record.rtd.sealed,
            func_name+": "+rtd.name.name+" is sealed (can't mutate)");

      record.set(k, val);
    };
  });

  // 6.4  Records: Inspection
  //(record? obj)    procedure
  define_libfunc("record?", 1, 1, function(ar){
    var obj = ar[0];
    if(BiwaScheme.isRecord(obj)){
      if(obj.rtd.opaque)
        return false; // opaque records pretend as if it is not a record.
      else
        return true;
    }
    else
      return false;
  });

  //(record-rtd record)    procedure
  define_libfunc("record-rtd", 1, 1, function(ar){
    assert_record(ar[0]);
    return ar[0].rtd;
  });

  //(record-type-name rtd)    procedure
  define_libfunc("record-type-name", 1, 1, function(ar){
    assert_record_td(ar[0]);
    return ar[0].name;
  });

  //(record-type-parent rtd)    procedure
  define_libfunc("record-type-parent", 1, 1, function(ar){
    assert_record_td(ar[0]);
    return ar[0].parent_rtd;
  });

  //(record-type-uid rtd)    procedure 
  define_libfunc("record-type-uid", 1, 1, function(ar){
    assert_record_td(ar[0]);
    return ar[0].uid;
  });

  //(record-type-generative? rtd)    procedure 
  define_libfunc("record-type-generative?", 1, 1, function(ar){
    assert_record_td(ar[0]);
    return ar[0].generative;
  });

  //(record-type-sealed? rtd)    procedure
  define_libfunc("record-type-sealed?", 1, 1, function(ar){
    assert_record_td(ar[0]);
    return ar[0].sealed;
  });

  //(record-type-opaque? rtd)    procedure
  define_libfunc("record-type-opaque?", 1, 1, function(ar){
    assert_record_td(ar[0]);
    return ar[0].opaque;
  });

  //(record-type-field-names rtd)    procedure
  define_libfunc("record-type-field-names", 1, 1, function(ar){
    assert_record_td(ar[0]);
    return _.map(ar[0].fields, function(field){ return field.name; });
  });

  //(record-field-mutable? rtd k)    procedure 
  define_libfunc("record-field-mutable?", 2, 2, function(ar){
    var rtd = ar[0], k = ar[1];
    assert_record_td(ar[0]);
    assert_integer(k);

    for(var _rtd = rtd.parent_rtd; _rtd; _rtd = _rtd.parent_rtd)
      k += _rtd.fields.length;

    return ar[0].fields[k].mutable;
  });

  //
  // Chapter 7 Exceptions and conditions
  //
//(with-exception-handler handler thunk)    procedure
//(guard (<variable>    syntax
  //(raise obj)    procedure 
  define_libfunc("raise", 1, 1, function(ar){
    throw new BiwaScheme.UserError(BiwaScheme.to_write(ar[0]));
  });
//(raise-continuable obj)    procedure
//
//&condition    condition type
//(condition condition1 ...)    procedure 
//(simple-conditions condition)    procedure
//(condition? obj)    procedure
//(condition-predicate rtd)    procedure 
//(condition-accessor rtd proc)    procedure
//
//&message    condition type 
//&warning    condition type 
//&serious    condition type 
//&error    condition type 
//&violation    condition type 
//&assertion    condition type 
//&irritants    condition type 
//&who    condition type 
//&non-continuable    condition type 
//&implementation-restriction    condition type 
//&lexical    condition type 
//&syntax    condition type 
//&undefined    condition type 

  //
  // Chapter 8 I/O
  //
//  //    8  I/O
//  //        8.1  Condition types
//&i/o    condition type 
//&i/o-read    condition type 
//&i/o-write    condition type 
//&i/o-invalid-position    condition type 
//&i/o-filename    condition type 
//&i/o-file-protection    condition type
//&i/o-file-is-read-only    condition type
//&i/o-file-already-exists    condition type 
//&i/o-file-does-not-exist    condition type
//&i/o-port    condition type 
//
//  //        8.2  Port I/O
//  //            8.2.1  File names
//  //(no function introduced)
//
//  //            8.2.2  File options
//(file-options <file-options symbol> ...)    syntax 
//
//  //            8.2.3  Buffer modes
//(buffer-mode <buffer-mode symbol>)    syntax  
//(buffer-mode? obj)    procedure
//
//  //            8.2.4  Transcoders
//(latin-1-codec)    procedure 
//(utf-8-codec)    procedure 
//(utf-16-codec)    procedure
//(eol-style <eol-style symbol>)    syntax
//(native-eol-style)    procedure
//&i/o-decoding    condition type
//&i/o-encoding    condition type 
//(error-handling-mode <error-handling-mode symbol>)    syntax 
//(make-transcoder codec)    procedure 
//(make-transcoder codec eol-style)    procedure 
//(make-transcoder codec eol-style handling-mode)    procedure
//(native-transcoder)    procedure
//(transcoder-codec transcoder)    procedure 
//(transcoder-eol-style transcoder)    procedure 
//(transcoder-error-handling-mode transcoder)    procedure 
//(bytevector->string bytevector transcoder)    procedure 
//(string->bytevector string transcoder)    procedure
//
  //            8.2.5  End-of-file object
  //-> 8.3 (eof-object)    procedure 
  //-> 8.3 (eof-object? obj)    procedure 

  //            8.2.6  Input and output ports
  define_libfunc("port?", 1, 1, function(ar){
    return (ar[0] instanceof Port);
  })
//(port-transcoder port)    procedure 
  define_libfunc("textual-port?", 1, 1, function(ar){
    assert_port(ar[0]);
    return !ar[0].is_binary;
  })
  define_libfunc("binary-port?", 1, 1, function(ar){
    assert_port(ar[0]);
    return ar[0].is_binary;
  })
//(transcoded-port binary-port transcoder)    procedure
//(port-has-port-position? port)    procedure 
//(port-position port)    procedure
//(port-has-set-port-position!? port)    procedure 
//(set-port-position! port pos)    procedure
  define_libfunc("close-port", 1, 1, function(ar){
    assert_port(ar[0]);
    ar[0].close();
    return BiwaScheme.undef;
  })
  //(call-with-port port proc)    procedure
  define_libfunc("call-with-port", 2, 2, function(ar){
    var port = ar[0], proc = ar[1];
    assert_port(port);
    assert_closure(proc);

    return new Call(proc, [port], function(ar){
      // Automatically close the port 
      port.close();
      return ar[0]; // TODO: values
    });
  });

  //            8.2.7  Input ports
  //8.3 (input-port? obj)    procedure 
//(port-eof? input-port)    procedure 
//(open-file-input-port filename)    procedure
//(open-bytevector-input-port bytevector)    procedure
//(open-string-input-port string)    procedure 
//(standard-input-port)    procedure 
//8.3 (current-input-port)    procedure
//(make-custom-binary-input-port id read!    procedure
//(make-custom-textual-input-port id read!    procedure
//
//  //            8.2.8  Binary input
//(get-u8 binary-input-port)    procedure
//(lookahead-u8 binary-input-port)    procedure
//(get-bytevector-n binary-input-port count)    procedure 
//(get-bytevector-n! binary-input-port    procedure
//(get-bytevector-some binary-input-port)    procedure
//(get-bytevector-all binary-input-port)    procedure
//
//  //            8.2.9  Textual input
//(get-char textual-input-port)    procedure
//(lookahead-char textual-input-port)    procedure 
//(get-string-n textual-input-port count)    procedure
//(get-string-n! textual-input-port string start count)    procedure
//(get-string-all textual-input-port)    procedure 
//(get-line textual-input-port)    procedure
//(get-datum textual-input-port)    procedure
//
  //            8.2.10  Output ports
  //8.3 (output-port? obj)    procedure
//(flush-output-port output-port)    procedure 
//(output-port-buffer-mode output-port)    procedure 
//(open-file-output-port filename)    procedure 
//(open-bytevector-output-port)    procedure 
//(call-with-bytevector-output-port proc)    procedure 
//(open-string-output-port)    procedure   
  //(call-with-string-output-port proc)    procedure 
  define_libfunc("call-with-string-output-port", 1, 1, function(ar){
    var proc = ar[0];
    assert_procedure(proc);

    var port = new BiwaScheme.Port.StringOutput();

    return new Call(proc, [port], function(ar){
      port.close();
      return port.output_string();
    });
  });

//(standard-output-port)    procedure 
//(standard-error-port)    procedure 
//8.3 (current-output-port)    procedure 
//8.3 (current-error-port)    procedure 
//(make-custom-binary-output-port id    procedure
  //(make-custom-textual-output-port id write! get-position set-position! close)
//  define_libfunc("make-custom-textual-output-port", 5, 5, function(ar){
//    assert_string(ar[0]);
//    assert_closure(ar[1]);
//    assert_closure(ar[2]);
//    assert_closure(ar[3]);
//    assert_closure(ar[4]);
//    return new Port(ar[0], ar[1], ar[2], ar[3], ar[4]);
//  })
//
//  //            8.2.11  Binary output
//(put-u8 binary-output-port octet)    procedure
//(put-bytevector binary-output-port bytevector)    procedure 
//
  //            8.2.12  Textual output
  define_libfunc("put-char", 2, 2, function(ar){
    assert_port(ar[0]);
    assert_char(ar[1]);
    ar[0].put_string(ar[1].value);
    return BiwaScheme.undef;
  })
  define_libfunc("put-string", 2, 2, function(ar){
    assert_port(ar[0]);
    assert_string(ar[1]);
    ar[0].put_string(ar[1]);
    return BiwaScheme.undef;
  })
  define_libfunc("put-datum", 2, 2, function(ar){
    assert_port(ar[0]);
    ar[0].put_string(to_write(ar[1]));
    return BiwaScheme.undef;
  })
//
//  //            8.2.13  Input/output ports
//(open-file-input/output-port filename)    procedure 
//(make-custom-binary-input/output-port    procedure 
//(make-custom-textual-input/output-port    procedure
//
//  //        8.3  Simple I/O
  define_libfunc("eof-object", 0, 0, function(ar){
    return eof;
  })
  define_libfunc("eof-object?", 1, 1, function(ar){
    return ar[0] === eof;
  })
//(call-with-input-file filename proc)    procedure 
//(call-with-output-file filename proc)    procedure
  define_libfunc("input-port?", 1, 1, function(ar){
    assert_port(ar[0]);
    return ar[0].is_input;
  })
  define_libfunc("output-port?", 1, 1, function(ar){
    assert_port(ar[0]);
    return ar[0].is_output;
  })
  define_libfunc("current-input-port", 0, 0, function(ar){
    return Port.current_input;
  })
  define_libfunc("current-output-port", 0, 0, function(ar){
    return Port.current_output;
  })
  define_libfunc("current-error-port", 0, 0, function(ar){
    return Port.current_error;
  })
//(with-input-from-file filename thunk)    procedure 
//(with-output-to-file filename thunk)    procedure
//(open-input-file filename)    procedure
//(open-output-file filename)    procedure 
  define_libfunc("close-input-port", 1, 1, function(ar){
    assert_port(ar[0]);
    if(!ar[0].is_input)
      throw new Error("close-input-port: port is not input port");
    ar[0].close();
    return BiwaScheme.undef;
  });
  define_libfunc("close-output-port", 1, 1, function(ar){
    assert_port(ar[0]);
    if(!ar[0].is_output)
      throw new Error("close-output-port: port is not output port");
    ar[0].close();
    return BiwaScheme.undef;
  });
//(read-char)    procedure 
//(peek-char)    procedure 
  define_libfunc("read", 0, 1, function(ar){
    var port = ar[0] || Port.current_input;
    assert_port(port);

    return port.get_string(function(str){
	    return Interpreter.read(str);
    });
  })

  define_libfunc("write-char", 1, 2, function(ar){
    var port = ar[1] || Port.current_output;
    assert_char(ar[0]);
    port.put_string(ar[0].value);
    return BiwaScheme.undef;
  });
  define_libfunc("newline", 0, 1, function(ar){
    var port = ar[0] || Port.current_output;
    port.put_string("\n");
    return BiwaScheme.undef;
  });
  define_libfunc("display", 1, 2, function(ar){
    var port = ar[1] || Port.current_output;
    port.put_string(to_display(ar[0]));
    return BiwaScheme.undef;
  });
  define_libfunc("write", 1, 2, function(ar){
    var port = ar[1] || Port.current_output;
    assert_port(port);
    port.put_string(to_write(ar[0]));
    return BiwaScheme.undef;
  });

  //
  // Chapter 9 File System
  // Chapter 10 Command-line access and exit values
  //
  // see src/library/node_functions.js

  //
  // Chapter 11 Arithmetic
  //
////        11.1  Bitwise operations
////        11.2  Fixnums
//(fixnum? obj)    procedure
//(fixnum-width)    procedure 
//(least-fixnum)    procedure 
//(greatest-fixnum)    procedure 
//(fx=? fx1 fx2 fx3 ...)    procedure 
//(fx>? fx1 fx2 fx3 ...)    procedure 
//(fx<? fx1 fx2 fx3 ...)    procedure 
//(fx>=? fx1 fx2 fx3 ...)    procedure 
//(fx<=? fx1 fx2 fx3 ...)    procedure 
//(fxzero? fx)    procedure 
//(fxpositive? fx)    procedure 
//(fxnegative? fx)    procedure 
//(fxodd? fx)    procedure 
//(fxeven? fx)    procedure
//(fxmax fx1 fx2 ...)    procedure 
//(fxmin fx1 fx2 ...)    procedure
//(fx+ fx1 fx2)    procedure 
//(fx* fx1 fx2)    procedure
//(fx- fx1 fx2)    procedure 
//(fxdiv-and-mod fx1 fx2)    procedure 
//(fxdiv fx1 fx2)    procedure 
//(fxmod fx1 fx2)    procedure 
//(fxdiv0-and-mod0 fx1 fx2)    procedure 
//(fxdiv0 fx1 fx2)    procedure 
//(fxmod0 fx1 fx2)    procedure 
//(fx+/carry fx1 fx2 fx3)    procedure
//(fx-/carry fx1 fx2 fx3)    procedure
//(fx*/carry fx1 fx2 fx3)    procedure 
//(fxnot fx)    procedure
//(fxand fx1 ...)    procedure 
//(fxior fx1 ...)    procedure 
//(fxxor fx1 ...)    procedure
//(fxif fx1 fx2 fx3)    procedure
//(fxbit-count fx)    procedure
//(fxlength fx)    procedure
//(fxfirst-bit-set fx)    procedure 
//(fxbit-set? fx1 fx2)    procedure
//(fxcopy-bit fx1 fx2 fx3)    procedure 
//(fxbit-field fx1 fx2 fx3)    procedure
//(fxcopy-bit-field fx1 fx2 fx3 fx4)    procedure
//(fxarithmetic-shift fx1 fx2)    procedure
//(fxarithmetic-shift-left fx1 fx2)    procedure 
//(fxarithmetic-shift-right fx1 fx2)    procedure
//(fxrotate-bit-field fx1 fx2 fx3 fx4)    procedure
//(fxreverse-bit-field fx1 fx2 fx3)    procedure
//
////        11.3  Flonums
//(flonum? obj)    procedure
//(real->flonum x)    procedure
//(fl=? fl1 fl2 fl3 ...)    procedure 
//(fl<? fl1 fl2 fl3 ...)    procedure 
//(fl<=? fl1 fl2 fl3 ...)    procedure 
//(fl>? fl1 fl2 fl3 ...)    procedure 
//(fl>=? fl1 fl2 fl3 ...)    procedure
//(flinteger? fl)    procedure 
//(flzero? fl)    procedure 
//(flpositive? fl)    procedure 
//(flnegative? fl)    procedure 
//(flodd? ifl)    procedure 
//(fleven? ifl)    procedure 
//(flfinite? fl)    procedure 
//(flinfinite? fl)    procedure 
//(flnan? fl)    procedure
//(flmax fl1 fl2 ...)    procedure 
//(flmin fl1 fl2 ...)    procedure
//(fl+ fl1 ...)    procedure 
//(fl* fl1 ...)    procedure 
//(fl- fl1 fl2 ...)    procedure 
//(fl- fl)    procedure 
//(fl/ fl1 fl2 ...)    procedure 
//(fl/ fl)    procedure 
//(flabs fl)    procedure
//(fldiv-and-mod fl1 fl2)    procedure 
//(fldiv fl1 fl2)    procedure 
//(flmod fl1 fl2)    procedure 
//(fldiv0-and-mod0 fl1 fl2)    procedure 
//(fldiv0 fl1 fl2)    procedure 
//(flmod0 fl1 fl2)    procedure 
//(flnumerator fl)    procedure 
//(fldenominator fl)    procedure 
//(flfloor fl)    procedure 
//(flceiling fl)    procedure 
//(fltruncate fl)    procedure 
//(flround fl)    procedure
//(flexp fl)    procedure 
//(fllog fl)    procedure 
//(fllog fl1 fl2)    procedure 
//(flsin fl)    procedure 
//(flcos fl)    procedure 
//(fltan fl)    procedure 
//(flasin fl)    procedure 
//(flacos fl)    procedure 
//(flatan fl)    procedure 
//(flatan fl1 fl2)    procedure
//(flsqrt fl)    procedure
//(flexpt fl1 fl2)    procedure 
//&no-infinities    condition type 
//&no-nans    condition type 
//(fixnum->flonum fx)    procedure 
//
////        11.4  Exact bitwise arithmetic
//(bitwise-not ei)    procedure
//(bitwise-and ei1 ...)    procedure 
//(bitwise-ior ei1 ...)    procedure 
//(bitwise-xor ei1 ...)    procedure 
//(bitwise-if ei1 ei2 ei3)    procedure
//(bitwise-bit-count ei)    procedure 
//(bitwise-length ei)    procedure
//(bitwise-first-bit-set ei)    procedure 
//(bitwise-bit-set? ei1 ei2)    procedure 
//(bitwise-copy-bit ei1 ei2 ei3)    procedure
//(bitwise-bit-field ei1 ei2 ei3)    procedure
//(bitwise-copy-bit-field ei1 ei2 ei3 ei4)    procedure 
//(bitwise-arithmetic-shift ei1 ei2)    procedure
//(bitwise-arithmetic-shift-left ei1 ei2)    procedure 
//(bitwise-arithmetic-shift-right ei1 ei2)    procedure 
//(bitwise-arithmetic-shift-right ei1 ei2)
//(bitwise-rotate-bit-field ei1 ei2 ei3 ei4)    procedure 
//(bitwise-reverse-bit-field ei1 ei2 ei3)    procedure 


  //
  // Chapter 12 syntax-case
  //

  //
  // Chapter 13 Hashtables
  //

  //13.1  Constructors
  //(define h (make-eq-hashtale)
  //(define h (make-eq-hashtable 1000))
  define_libfunc("make-eq-hashtable", 0, 1, function(ar){
    // Note: ar[1] (hashtable size) is just ignored
    return new Hashtable(Hashtable.eq_hash, Hashtable.eq_equiv);
  });
  //(make-eqv-hashtable)    procedure 
  //(make-eqv-hashtable k)    procedure 
  define_libfunc("make-eqv-hashtable", 0, 1, function(ar){
    return new Hashtable(Hashtable.eqv_hash, Hashtable.eqv_equiv);
  });
  //(make-hashtable hash-function equiv)    procedure 
  //(make-hashtable hash-function equiv k)    procedure
  define_libfunc("make-hashtable", 2, 3, function(ar){
    assert_procedure(ar[0]);
    assert_procedure(ar[1]);
    return new Hashtable(ar[0], ar[1]);
  });

  //13.2  Procedures
  // (hashtable? hash)
  define_libfunc("hashtable?", 1, 1, function(ar){
    return ar[0] instanceof Hashtable;
  });
  //(hashtable-size hash)
  define_libfunc("hashtable-size", 1, 1, function(ar){
    assert_hashtable(ar[0]);
    return ar[0].keys().length;
  });

  // Find a pair from a hashtable with given key.
  //
  // hash      - a BiwaScheme.Hashtable
  // key       - an object
  // callbacks - an object contains callback functions
  //             .on_found     - function(pair, hashed)
  //               pair   - [Object key, Object value]
  //               hashed - Object hashed
  //             .on_not_found - function(hashed)
  //               hashed - Object hashed
  //
  // Returns an instance of BiwaScheme.Call.
  BiwaScheme.find_hash_pair = function(hash, key, callbacks){
    // invoke hash proc 
    return new Call(hash.hash_proc, [key], function(ar){
      var hashed = ar[0];
      var candidate_pairs = hash.candidate_pairs(hashed);

      if (!candidate_pairs){ // shortcut: obviously not found
        return callbacks.on_not_found(hashed);
      }

      // search the exact key from candidates
      return Call.foreach(candidate_pairs, {
        call: function(pair){
          // invoke the equivalence proc
          return new Call(hash.equiv_proc, [key, pair[0]]);
        },
        result: function(equal, pair){
          if(equal) {       // found
            return callbacks.on_found(pair, hashed);
          }
        },
        finish: function(){ // not found
          return callbacks.on_not_found(hashed);
        }
      });
    });
  };

  //(hashtable-ref hash "foo" #f)
  define_libfunc("hashtable-ref", 3, 3, function(ar){
    var hash = ar[0], key = ar[1], ifnone = ar[2];
    assert_hashtable(hash);

    return BiwaScheme.find_hash_pair(hash, key, {
      on_found: function(pair){
        return pair[1];
      },
      on_not_found: function(hashed){
        return ifnone;
      }
    });
  });

  //(hashtable-set! hash "foo" '(1 2))
  define_libfunc("hashtable-set!", 3, 3, function(ar){
    var hash = ar[0], key = ar[1], value = ar[2];
    assert_hashtable(hash);
    assert(hash.mutable, "hashtable is not mutable");

    return BiwaScheme.find_hash_pair(hash, key, {
      on_found: function(pair){
        pair[1] = value;
        return BiwaScheme.undef;
      },
      on_not_found: function(hashed){
        hash.add_pair(hashed, key, value);
        return BiwaScheme.undef;
      }
    });
  });

  //(hashtable-delete! hash "foo")
  define_libfunc("hashtable-delete!", 2, 2, function(ar){
    var hash = ar[0], key = ar[1];
    assert_hashtable(hash);
    assert(hash.mutable, "hashtable is not mutable");

    return BiwaScheme.find_hash_pair(hash, key, {
      on_found: function(pair, hashed){
        hash.remove_pair(hashed, pair);
        return BiwaScheme.undef;
      },
      on_not_found: function(hashed){
        return BiwaScheme.undef;
      }
    });
  });

  //(hashtable-contains? hash "foo")
  define_libfunc("hashtable-contains?", 2, 2, function(ar){
    var hash = ar[0], key = ar[1];
    assert_hashtable(hash);

    return BiwaScheme.find_hash_pair(hash, key, {
      on_found: function(pair){
        return true;
      },
      on_not_found: function(hashed){
        return false;
      }
    });
  });

  //(hashtable-update! hashtable key proc default)    procedure 
  define_libfunc("hashtable-update!", 4, 4, function(ar){
    var hash = ar[0], key = ar[1], proc = ar[2], ifnone = ar[3];
    assert_hashtable(hash);
    assert(hash.mutable, "hashtable is not mutable");
    assert_procedure(proc);

    return BiwaScheme.find_hash_pair(hash, key, {
      on_found: function(pair, hashed){
        // invoke proc and get new value
        return new Call(proc, [pair[1]], function(ar){
          // replace the value
          pair[1] = ar[0];
          return BiwaScheme.undef;
        });
      },
      on_not_found: function(hashed){
        // invoke proc and get new value
        return new Call(proc, [ifnone], function(ar){
          // create new pair
          hash.add_pair(hashed, key, ar[0]);
          return BiwaScheme.undef;
        });
      }
    });
  });
  //(hashtable-copy hashtable)    procedure 
  //(hashtable-copy hashtable mutable)    procedure 
  define_libfunc("hashtable-copy", 1, 2, function(ar){
    var mutable = (ar[1]===undefined) ? false : !!ar[1];
    assert_hashtable(ar[0]);
    return ar[0].create_copy(mutable);
  });
  //(hashtable-clear! hashtable)    procedure 
  //(hashtable-clear! hashtable k)    procedure 
  define_libfunc("hashtable-clear!", 0, 1, function(ar){
    assert_hashtable(ar[0]);
    assert(ar[0].mutable, "hashtable is not mutable");
    ar[0].clear();
    return BiwaScheme.undef;
  });
  //(hashtable-keys hash)  ; => vector
  define_libfunc("hashtable-keys", 1, 1, function(ar){
    assert_hashtable(ar[0]);
    return ar[0].keys();
  });
  //(hashtable-entries hash)  ; => two vectors (keys, values)
  define_libfunc("hashtable-entries", 1, 1, function(ar){
    assert_hashtable(ar[0]);
    return new Values([ar[0].keys(), ar[0].values()]);
  });

  //13.3  Inspection
  
  //(hashtable-equivalence-function hashtable)    procedure 
  define_libfunc("hashtable-equivalence-function", 1, 1, function(ar){
    assert_hashtable(ar[0]);
    return ar[0].equiv_proc;
  });
  //(hashtable-hash-function hashtable)    procedure 
  define_libfunc("hashtable-hash-function", 1, 1, function(ar){
    assert_hashtable(ar[0]);
    return ar[0].hash_proc;
  });
  //(hashtable-mutable? hashtable)    procedure 
  define_libfunc("hashtable-mutable?", 1, 1, function(ar){
    assert_hashtable(ar[0]);
    return ar[0].mutable;
  });

  //13.4  Hash functions

  //(equal-hash obj)    procedure 
  define_libfunc("equal-hash", 0, 0, function(ar){
    return Hashtable.equal_hash;
  });
  //(string-hash string)    procedure
  define_libfunc("string-hash", 0, 0, function(ar){
    return Hashtable.string_hash;
  });
  //(string-ci-hash string)    procedure
  define_libfunc("string-ci-hash", 0, 0, function(ar){
    return Hashtable.string_ci_hash;
  });
  //(symbol-hash symbol)    procedure
  define_libfunc("symbol-hash", 0, 0, function(ar){
    return Hashtable.symbol_hash;
  });

  //
  // Chapter 14 Enumerators
  //
  //(make-enumeration symbol-list) -> enum-set(new type)
  define_libfunc("make-enumeration", 1, 1, function(ar){
    assert_list(ar[0]);
    var members = ar[0].to_array();
    var enum_type = new BiwaScheme.Enumeration.EnumType(members);
    return enum_type.universe();
  });

  //(enum-set-universe enum-set) -> enum-set(same type as the argument)
  define_libfunc("enum-set-universe", 1, 1, function(ar){
    assert_enum_set(ar[0]);
    return ar[0].enum_type.universe();
  });

  //(enum-set-indexer enum-set) -> (lambda (sym)) -> integer or #f
  define_libfunc("enum-set-indexer", 1, 1, function(ar){
    assert_enum_set(ar[0]);
    return ar[0].enum_type.indexer();
  });

  //(enum-set-constructor enum-set) -> (lambda (syms)) -> enum-set(same type as the argument)
  define_libfunc("enum-set-constructor", 1, 1, function(ar){
    assert_enum_set(ar[0]);
    return ar[0].enum_type.constructor();
  });

  //(enum-set->list enum-set) -> symbol-list
  define_libfunc("enum-set->list", 1, 1, function(ar){
    assert_enum_set(ar[0]);
    return ar[0].symbol_list();
  });

  //(enum-set-member? symbol enum-set) -> bool
  define_libfunc("enum-set-member?", 2, 2, function(ar){
    assert_symbol(ar[0]);
    assert_enum_set(ar[1]);
    return ar[1].is_member(ar[0]);
  });

  //(enum-set-subset? esa esb) -> bool
  define_libfunc("enum-set-subset?", 2, 2, function(ar){
    assert_enum_set(ar[0]);
    assert_enum_set(ar[1]);
    return ar[0].is_subset(ar[1]);
  });

  //(enum-set=? esa esb) -> bool
  define_libfunc("enum-set=?", 2, 2, function(ar){
    assert_enum_set(ar[0]);
    assert_enum_set(ar[1]);
    return ar[0].equal_to(ar[1]);
  });

  //(enum-set-union es1 es2) -> enum-set
  define_libfunc("enum-set-union", 2, 2, function(ar){
    assert_enum_set(ar[0]);
    assert_enum_set(ar[1]);
    assert(ar[0].enum_type === ar[1].enum_type,
           "two enum-sets must be the same enum-type", "enum-set-union");
    return ar[0].union(ar[1]);
  });

  //(enum-set-intersection es1 es2) -> enum-set
  define_libfunc("enum-set-intersection", 2, 2, function(ar){
    assert_enum_set(ar[0]);
    assert_enum_set(ar[1]);
    return ar[0].intersection(ar[1]);
  });

  //(enum-set-difference es1 es2) -> enum-set
  define_libfunc("enum-set-difference", 2, 2, function(ar){
    assert_enum_set(ar[0]);
    assert_enum_set(ar[1]);
    return ar[0].difference(ar[1]);
  });

  //(enum-set-complement enum-set) -> enum-set
  define_libfunc("enum-set-complement", 1, 1, function(ar){
    assert_enum_set(ar[0]);
    return ar[0].complement();
  });

  //(enum-set-projection esa esb) -> enum-set
  define_libfunc("enum-set-projection", 2, 2, function(ar){
    assert_enum_set(ar[0]);
    assert_enum_set(ar[1]);
    return ar[0].projection(ar[1]);
  });

  //(define-enumeration <type-name> (<symbol> ...) <constructor-syntax>)
  // Example: 
  //   (define-enumeration color (red green black white) color-set)
  //   this defines:
  //     - an EnumType
  //     - (color red) ;=> 'red
  //     - (color-set red black) ;=> #<enum-set (red black)>
  define_syntax("define-enumeration", function(x){
    // Extract parameters 
    var type_name = x.cdr.car;
    assert(BiwaScheme.isSymbol(type_name),
           "expected symbol for type_name", "define-enumeration");
    type_name = type_name.name;

    var members = x.cdr.cdr.car;
    assert(BiwaScheme.isList(members),
           "expected list of symbol for members", "define-enumeration");
    members = members.to_array();

    var constructor_name = x.cdr.cdr.cdr.car;
    assert(BiwaScheme.isSymbol(constructor_name),
           "expected symbol for constructor_name", "define-enumeration");
    constructor_name = constructor_name.name;

    // Define EnumType
    var enum_type = new BiwaScheme.Enumeration.EnumType(members);
    
    // Define (color red)
    define_syntax(type_name, function(x){
      // (color)
      assert(!BiwaScheme.isNil(x.cdr),
             "an argument is needed", type_name);

      var arg = x.cdr.car;
      assert_symbol(arg, type_name);
      
      // Check arg is included in the universe
      assert(_.include(enum_type.members, arg),
        arg.name+" is not included in the universe: "+
          BiwaScheme.to_write(enum_type.members),
        type_name);

      return BiwaScheme.List(Sym("quote"), arg);
    });

    // Define (color-set red black)
    define_syntax(constructor_name, function(x){
      assert_list(x.cdr, constructor_name);

      var symbols = x.cdr.to_array();

      // Check each argument is included in the universe
      _.each(symbols, function(arg){
        assert_symbol(arg, constructor_name);
        assert(_.include(enum_type.members, arg),
          arg.name+" is not included in the universe: "+
            BiwaScheme.to_write(enum_type.members),
          constructor_name);
      });

      // Create an EnumSet
      return new BiwaScheme.Enumeration.EnumSet(enum_type, symbols);
    });
  });

  //
  // Chapter 15 Composite library
  //
  //(rnrs 6) = all - eval - mutable pairs - mutable strings - r5rs compatibility

  //
  // Chapter 16 eval
  //
  //(eval expression environment)    procedure 
  define_libfunc("eval", 1, 1, function(ar, intp){
    //TODO: environment
    //TODO: this implementation has a bug that
    //  expressions which contains #<undef>, etc. cannot be evaluated.
    var expr = ar[0];
    var intp2 = new Interpreter(intp);
    return intp2.evaluate(expr.to_write());
  });
//(environment import-spec ...)    procedure

  //
  // Chapter 17 Mutable pairs
  //
//(set-car! pair obj)    procedure
//(set-cdr! pair obj)    procedure

  //
  // Chapter 18 Mutable strings
  //
  //(string-set! string k char)    procedure
 // (string-fill! string char)    procedure 
 
  //
  // Chapter 19 R5RS compatibility
  //
//(exact->inexact z)    procedure 
//(inexact->exact z)    procedure 
//
//(quotient n1 n2)    procedure 
//(remainder n1 n2)    procedure 
//(modulo n1 n2)    procedure
//
//(delay <expression>)    syntax  
//(force promise)    procedure 
//(make-promise (lambda () <expression>))
//
//(null-environment n)    procedure 
//(scheme-report-environment n)    procedure 

  /* --------------------------------------- namespace webscheme */ 
}
if( typeof(BiwaScheme)!='object' ) BiwaScheme={}; with(BiwaScheme) {

  //
  // interface to javascript
  //
  define_libfunc("js-eval", 1, 1, function(ar){
    return eval(ar[0]);
  });
  define_libfunc("js-ref", 2, 2, function(ar){
    if(_.isString(ar[1])){
      return ar[0][ar[1]];
    }
    else{
      assert_symbol(ar[1]);
      return ar[0][ar[1].name];
    }
  });
  define_libfunc("js-set!", 3, 3, function(ar){
    assert_string(ar[1]);
    ar[0][ar[1]] = ar[2];
    return BiwaScheme.undef;
  });

  // (js-call (js-eval "Math.pow") 2 4)
  define_libfunc("js-call", 1, null, function(ar){
    var js_func = ar.shift();
    assert_function(js_func);

    var receiver = null;
    return js_func.apply(receiver, ar);
  });
  // (js-invoke (js-new "Date") "getTime")
  define_libfunc("js-invoke", 2, null, function(ar){
    var js_obj = ar.shift();
    var func_name = ar.shift();
    if(!_.isString(func_name)){
      assert_symbol(func_name);
      func_name = func_name.name;
    }
    if(js_obj[func_name])
      return js_obj[func_name].apply(js_obj, ar);
    else
      throw new Error("js-invoke: function "+func_name+" is not defined");
  });

  // Short hand for JavaScript method call.
  //
  // (js-invocation obj '(foo 1 2 3))  ;=> obj.foo(1,2,3)
  // (js-invocation obj '(foo 1 2 3)   ;=> obj.foo(1,2,3)
  //                    'bar           ;      .bar
  //                    '(baz 4 5))    ;      .baz(4,5)
  // (js-invocation 'Math '(pow 2 3))  ;=> Math.pow(2,3)
  //
  // It also converts
  //   (lambda (e) ...) to
  //   (js-closure (lambda (e) ...))
  //   and
  //   '((a . b) (c . 4)) to
  //   {a: "b", c: 4}
  //
  // TODO: provide corresponding macro ".." 
  define_libfunc("js-invocation", 2, null, function(ar, intp){
    var receiver = ar.shift();
    // TODO: convert lambdas by js-closure 
    if(BiwaScheme.isSymbol(receiver)){
      receiver = eval(receiver.name); //XXX: is this ok?
    }

    var v = receiver;

    // Process each method call
    _.each(ar, function(callspec){
        if(BiwaScheme.isSymbol(callspec)){
          // Property access
          v = v[callspec.name];
        }
        else if(BiwaScheme.isList(callspec)){
          // Method call
          var args = callspec.to_array();

          assert_symbol(args[0]);
          var method = args.shift().name;

          // Convert arguments
          args = _.map(args, function(arg){
              if(BiwaScheme.isClosure(arg)){
                // closure -> JavaScript funciton
                return BiwaScheme.js_closure(arg, intp);
              }
              else if(BiwaScheme.isList(arg)){
                // alist -> JavaScript Object
                var o = {};
                arg.foreach(function(pair){
                    assert_symbol(pair.car);
                    o[pair.car.name] = pair.cdr;
                  });
                return o;
              }
              else
                return arg;
            });

          // Call the method
          if(!_.isFunction(v[method])){
            throw new BiwaScheme.Error("js-invocation: the method `"+method+"' not found");
          }
          v = v[method].apply(v, args);
        }
        else{
          // (wrong argument)
          throw new BiwaScheme.Error("js-invocation: expected list or symbol for callspec but got " + BiwaScheme.inspect(callspec));
        }
      });

    return v;
  });

  // (js-new "Date" 2005 1 1)
  // (js-new "Draggable elem 'onEnd (lambda (drg) ...))
  //   If symbol is given, following arguments are converted to 
  //   an js object. If any of them is a scheme closure,
  //   it is converted to js function which invokes that closure.
  //
  define_libfunc("js-new", 1, null, function(ar, intp){
    // make js object from key-value pair
    var array_to_obj = function(ary){
      if((ary.length % 2) != 0)
        throw new Error("js-new: odd number of key-value pair");

      var obj = {};
      for(var i=0; i<ary.length; i+=2){
        var key = ary[i], value = ary[i+1];
        assert_symbol(key);
        if(value.closure_p === true)
          value = BiwaScheme.js_closure(value, intp);

        obj[key.name] = value;
      }
      return obj;
    };

    var ctor = ar.shift();
    assert_string(ctor);
    if(ar.length == 0){
      return eval("new " + ctor + "()");
    }
    else{
      // pack args to js object, if symbol appears
      var args = [];
      for(var i=0; i<ar.length; i++){
        if(ar[i] instanceof Symbol){
          args.push(array_to_obj(ar.slice(i)));
          break;
        }
        else{
          args.push(ar[i]);
        }
      }
      // construct js code to create new object
      var args_str = _.map(ar, function(value, i){
        return "args['" + i + "']";
      }).join(",");
      return eval("new " + ctor + "(" + args_str + ")");
    }
  });

  // (js-obj "foo" 1 "bar" 2)
  // -> {"foo": 1, "bar": 2}
  define_libfunc("js-obj", 0, null, function(ar){
    if(ar.length % 2 != 0){
      throw new Error("js-obj: number of arguments must be even");
    }

    var obj = {};
    for(i=0; i<ar.length/2; i++){
      assert_string(ar[i*2]);
      obj[ar[i*2]] = ar[i*2+1];
    }
    return obj;
  });

  BiwaScheme.js_closure = function(proc, intp){
    var intp2 = new Interpreter(intp);
    return function(/*args*/){
      return intp2.invoke_closure(proc, _.toArray(arguments));
    };
  };
  // (js-closure (lambda (event) ..))
  // Returns a js function which executes the given procedure.
  //
  // Example
  //   (add-handler! ($ "#btn") "click" (js-closure on-click))
  define_libfunc("js-closure", 1, 1, function(ar, intp){
    assert_closure(ar[0]);
    return BiwaScheme.js_closure(ar[0], intp);
  });

  define_libfunc("js-null?", 1, 1, function(ar){
    return ar[0] === null;
  });

  define_libfunc("js-undefined?", 1, 1, function(ar){
    return ar[0] === undefined;
  });

  define_libfunc("js-function?", 1, 1, function(ar){
    return _.isFunction(ar[0]);
  });

  define_libfunc("js-array-to-list", 1, 1, function(ar){
    return BiwaScheme.array_to_list(ar[0]);
  });

  define_libfunc("list-to-js-array", 1, 1, function(ar){
    return ar[0].to_array();
  });

  BiwaScheme.alist_to_js_obj = function(alist) {
    if (alist === nil) {
      return {} ;
    }
    assert_list(alist);
    var obj = {};
    alist.foreach(function(item){
      assert_string(item.car);
      obj[item.car] = item.cdr;
    });
    return obj;
  };
  define_libfunc("alist-to-js-obj", 1, 1, function(ar) {
    return BiwaScheme.alist_to_js_obj(ar[0]);
  });

  BiwaScheme.js_obj_to_alist = function(obj) {
    if (obj === undefined) {
      return BiwaScheme.nil;
    }
    var arr = [];
    _.each(obj, function(val, key) {
      arr.push(new Pair(key, val));
    });
    var alist = BiwaScheme.array_to_list(arr);
    return alist;
  };
  define_libfunc("js-obj-to-alist", 1, 1, function(ar) {
    return BiwaScheme.js_obj_to_alist(ar[0]);
  });

  //
  // timer, sleep
  //
  define_libfunc("timer", 2, 2, function(ar, intp){
    var proc = ar[0], sec = ar[1];
    assert_closure(proc);
    assert_real(sec);
    var intp2 = new Interpreter(intp);
    setTimeout(function(){ intp2.invoke_closure(proc); }, sec * 1000);
    return BiwaScheme.undef;
  });
  define_libfunc("set-timer!", 2, 2, function(ar, intp){
    var proc = ar[0], sec = ar[1];
    assert_closure(proc);
    assert_real(sec);
    var intp2 = new Interpreter(intp);
    return setInterval(function(){ intp2.invoke_closure(proc); }, sec * 1000);
  });
  define_libfunc("clear-timer!", 1, 1, function(ar){
    var timer_id = ar[0];
    clearInterval(timer_id);
    return BiwaScheme.undef;
  });
  define_libfunc("sleep", 1, 1, function(ar){
    var sec = ar[0];
    assert_real(sec);
    return new BiwaScheme.Pause(function(pause){
      setTimeout(function(){ pause.resume(nil); }, sec * 1000);
    });
  });

  //
  // console
  //
  // (console-debug obj1 ...)
  // (console-log obj1 ...)
  // (console-info obj1 ...)
  // (console-warn obj1 ...)
  // (console-error obj1 ...)
  //   Put objects to console, if window.console is defined.
  //   Returns obj1.
  //
  // Example:
  //     (some-func arg1 (console-debug arg2) arg3)
  var define_console_func = function(name){
    define_libfunc("console-"+name, 1, null, function(ar){
      var con = window.console;
      if(con){
        var vals = _.map(ar, function(item){
          return BiwaScheme.inspect(item, {fallback: item});
        });

        con[name].apply(con, vals);
      }
      return ar[0];
    });
  };
  define_console_func("debug");
  define_console_func("log");
  define_console_func("info");
  define_console_func("warn");
  define_console_func("error");

}

if( typeof(BiwaScheme)!='object' ) BiwaScheme={}; with(BiwaScheme) {
  define_libfunc("html-escape", 1, 1, function(ar){
    assert_string(ar[0]);
    return _.escape(ar[0]);
  });
  BiwaScheme.inspect_objs = function(objs){
    return _.map(objs, BiwaScheme.inspect).join(", ");
  };
  define_libfunc("inspect", 1, null, function(ar){
    return BiwaScheme.inspect_objs(ar);
  });
  define_libfunc("inspect!", 1, null, function(ar){
    puts(BiwaScheme.inspect_objs(ar));
    return BiwaScheme.undef;
  });

  //
  // json
  //
  // json->sexp
  // Array -> list
  // Object -> alist
  // (number, boolean, string, 
  //
  BiwaScheme.json2sexp = function(json){
    switch(true){
    case _.isNumber(json) ||
         _.isString(json) ||
         json === true || json === false:
      return json;
    case _.isArray(json):
      return array_to_list(_.map(json, json2sexp));
    case typeof(json) == "object":
      var ls = nil;
      for(key in json){
        ls = new Pair(new Pair(key, json2sexp(json[key])),
               ls);
      }
      return ls;
    default:
      throw new Error("json->sexp: detected invalid value for json: "+BiwaScheme.inspect(json));
    }
    throw new Bug("must not happen");
  }
  define_libfunc("json->sexp", 1, 1, function(ar){
    return json2sexp(ar[0]);
  })

  //
  //from Gauche
  //

  // (identity obj)
  // Returns obj.
  define_libfunc("identity", 1, 1, function(ar){
    return ar[0];
  });

  // (inc! i)
  // = (set! i (+ i 1))
  // Increments i (i.e., set i+1 to i).
  define_syntax("inc!", function(x){
    var target = x.cdr.car;
    return List(Sym("set!"),
                target, 
                [Sym("+"), target, 1]);
  });
  
  // (dec! i)
  // = (set! i (- i 1))
  // Decrements i (i.e., set i-1 to i).
  define_syntax("dec!", function(x){
    var target = x.cdr.car;
    return List(Sym("set!"),
                target, 
                [Sym("-"), target, 1]);
  });

  // string
  
  define_libfunc("string-concat", 1, 1, function(ar){
    assert_list(ar[0]);
    return ar[0].to_array().join("");
  })

  define_libfunc("string-split", 2, 2, function(ar){
    assert_string(ar[0]);
    assert_string(ar[1]);
    return array_to_list(ar[0].split(ar[1]));
  })

  define_libfunc("string-join", 1, 2, function(ar){
    assert_list(ar[0]);
    var delim = ""
    if(ar[1]){
      assert_string(ar[1]);
      delim = ar[1];
    }
    return ar[0].to_array().join(delim);
  })
  
  // lists

  define_libfunc("intersperse", 2, 2, function(ar){
    var item = ar[0], ls = ar[1];
    assert_list(ls);

    var ret = [];
    _.each(ls.to_array().reverse(),function(x){
      ret.push(x);
      ret.push(item);
    });
    ret.pop();
    return array_to_list(ret);
  });

  define_libfunc("map-with-index", 2, null, function(ar){
    var proc = ar.shift(), lists = ar;
    _.each(lists, assert_list);

    var results = [], i = 0;
    return Call.multi_foreach(lists, {
      call: function(xs){ 
        var args = _.map(xs, function(x){ return x.car });
        args.unshift(i);
        i++;
        return new Call(proc, args);
      },
      result: function(res){ results.push(res); },
      finish: function(){ return array_to_list(results); }
    });
  });

  // loop

  // (dotimes (variable limit result) body ...)
  // Iterate with variable 0 to limit-1.
  // ->
  //    (do ((tlimit limit)
  //         (variable 0 (+ variable 1)))
  //        ((>= variable tlimit) result)
  //      body ...)
  define_syntax("dotimes", function(x){
    var spec = x.cdr.car,
        bodies = x.cdr.cdr;
    var variable = spec.car,
        limit = spec.cdr.car,
        result = spec.cdr.cdr.car;
    var tlimit = BiwaScheme.gensym();

    var do_vars = deep_array_to_list([[tlimit, limit],
                                      [variable, 0, [Sym("+"), variable, 1]]]);
    var do_check = deep_array_to_list([[Sym(">="), variable, tlimit], result]);

    return new Pair(Sym("do"),
             new Pair(do_vars,
               new Pair(do_check,
                 bodies)));
  });

  // sorting
  
  // These functions takes a Scheme proc and sort the given
  // list or vector using the proc as the compare function.
  //
  // Limitations:
  //  - you should not use Ajax or sleep inside the proc
  //  - you cannot access to the free variables outside the proc
  //
  // (list-sort/comp proc list)
  // (vector-sort/comp proc vector)
  // (vector-sort/comp! proc vector)

  // utility function. takes a JS Array and a Scheme procedure,
  // returns sorted array
  var sort_with_comp = function(ary, proc, intp){
    return ary.sort(function(a, b){
        var intp2 = new BiwaScheme.Interpreter(intp);
        return intp2.invoke_closure(proc, [a, b]);
      });
  };

  define_libfunc("list-sort/comp", 1, 2, function(ar, intp){
    assert_procedure(ar[0]);
    assert_list(ar[1]);

    return array_to_list(sort_with_comp(ar[1].to_array(), ar[0], intp));
  });
  define_libfunc("vector-sort/comp", 1, 2, function(ar, intp){
    assert_procedure(ar[0]);
    assert_vector(ar[1]);

    return sort_with_comp(_.clone(ar[1]), ar[0], intp);
  });
  define_libfunc("vector-sort/comp!", 1, 2, function(ar, intp){
    assert_procedure(ar[0]);
    assert_vector(ar[1]);

    sort_with_comp(ar[1], ar[0], intp);
    return BiwaScheme.undef;
  });
  
  // macros

  //(define-macro (foo x) body ...)
  //(define-macro foo lambda)

  var rearrange_args = function (expected, given) {
    var args = [];
    var dotpos = (new Compiler).find_dot_pos(expected);
    if (dotpos == -1)
      args = given;
    else {
      for (var i = 0; i < dotpos; i++) {
        args[i] = given[i];
      }
      args[i] = array_to_list(given.slice(i));
    }
    return args;
  }
  define_syntax("define-macro", function(x){
    var head = x.cdr.car;
    var expected_args;
    if(head instanceof Pair){
      var name = head.car;
      expected_args = head.cdr;
      var body = x.cdr.cdr;
      var lambda = new Pair(Sym("lambda"),
                     new Pair(expected_args,
                       body))
    }
    else{
      var name = head;
      var lambda = x.cdr.cdr.car;
      expected_args = lambda.cdr.car;
    }

    //[close, n_frees, do_body, next]
    var opc = Compiler.compile(lambda);
    if(opc[1] != 0)
      throw new Bug("you cannot use free variables in macro expander (or define-macro must be on toplevel)")
    var cls = [opc[2]];

    TopEnv[name.name] = new Syntax(name.name, function(sexp){
      var given_args = sexp.to_array();

      given_args.shift();
      
      var intp = new Interpreter();
      var args = rearrange_args(expected_args, given_args);
      var result = intp.invoke_closure(cls, args);
      return result;
    });

    return BiwaScheme.undef;
  })

  var macroexpand_1 = function(x){
    if(x instanceof Pair){
      if(x.car instanceof Symbol && TopEnv[x.car.name] instanceof Syntax){
        var transformer = TopEnv[x.car.name];
        x = transformer.transform(x);
      }
      else
        throw new Error("macroexpand-1: `" + to_write_ss(x) + "' is not a macro");
    }
    return x;
  }
  define_syntax("%macroexpand", function(x){
    var expanded = BiwaScheme.Interpreter.expand(x.cdr.car);
    return List(Sym("quote"), expanded);
  });
  define_syntax("%macroexpand-1", function(x){
    var expanded = macroexpand_1(x.cdr.car);
    return List(Sym("quote"), expanded);
  });

  define_libfunc("macroexpand", 1, 1, function(ar){
    return BiwaScheme.Interpreter.expand(ar[0]);
  });
  define_libfunc("macroexpand-1", 1, 1, function(ar){
    return macroexpand_1(ar[0]);
  });

  define_libfunc("gensym", 0, 0, function(ar){
    return BiwaScheme.gensym();
  });
  
  // i/o

  define_libfunc("print", 1, null, function(ar){
    _.map(ar, function(item){
      puts(to_display(item), true);
    })
    puts(""); //newline
    return BiwaScheme.undef;
  })
  define_libfunc("write-to-string", 1, 1, function(ar){
    return to_write(ar[0]);
  });
  define_libfunc("read-from-string", 1, 1, function(ar){
    assert_string(ar[0]);
    return Interpreter.read(ar[0]);
  });
  define_libfunc("port-closed?", 1, 1, function(ar){
    assert_port(ar[0]);
    return !(ar[0].is_open);
  });
  //define_libfunc("with-input-from-port", 2, 2, function(ar){
  //define_libfunc("with-error-to-port", 2, 2, function(ar){
  define_libfunc("with-output-to-port", 2, 2, function(ar){
    var port = ar[0], proc = ar[1];
    assert_port(port);
    assert_procedure(proc);

    var original_port = BiwaScheme.Port.current_output;
    BiwaScheme.Port.current_output = port

    return new Call(proc, [port], function(ar){
      port.close();
      BiwaScheme.Port.current_output = original_port;

      return ar[0];
    });
  });
  
  // syntax
  
  define_syntax("let1", function(x){
    //(let1 vari expr body ...) 
    //=> ((lambda (var) body ...) expr)
    var vari = x.cdr.car; 
    var expr = x.cdr.cdr.car;
    var body = x.cdr.cdr.cdr;

    return new Pair(new Pair(Sym("lambda"),
                      new Pair(new Pair(vari, nil),
                        body)),
             new Pair(expr, nil));
  })

  //
  // Regular Expression
  //
  var assert_regexp = function(obj, fname){
    if(!(obj instanceof RegExp))
      throw new Error(fname + ": regexp required, but got " + to_write(obj));
  }

  //Function: string->regexp string &keyword case-fold 
  define_libfunc("string->regexp", 1, 1, function(ar){
    assert_string(ar[0], "string->regexp");
    return new RegExp(ar[0]); //todo: case-fold
  })
  //Function: regexp? obj 
  define_libfunc("regexp?", 1, 1, function(ar){
    return (ar[0] instanceof RegExp);
  })
  //Function: regexp->string regexp 
  define_libfunc("regexp->string", 1, 1, function(ar){
    assert_regexp(ar[0], "regexp->string");
    return ar[0].toString().slice(1, -1); //cut '/' 
  })

  define_libfunc("regexp-exec", 2, 2, function(ar){
    var rexp = ar[0];
    if(_.isString(ar[0])){
      rexp = new RegExp(ar[0]);
    }
    assert_regexp(rexp, "regexp-exec");
    assert_string(ar[1], "regexp-exec");
    var ret = rexp.exec(ar[1])
    return (ret === null) ? false : array_to_list(ret);
  })

//  //Function: rxmatch regexp string 
//  define_libfunc("rxmatch", 1, 1, function(ar){
//    assert_regexp(ar[0], "rxmatch");
//    assert_string(ar[1], "rxmatch");
//    return ar[0].match(ar[1]);
//  });
  //Function: rxmatch-start match &optional (i 0) 
  //Function: rxmatch-end match &optional (i 0) 
  //Function: rxmatch-substring match &optional (i 0) 
  //Function: rxmatch-num-matches match   
  //Function: rxmatch-after match &optional (i 0) 
  //Function: rxmatch-before match &optional (i 0) 
  //Generic application: regmatch &optional index 
  //Generic application: regmatch 'before &optional index 
  //Generic application: regmatch 'after &optional index 
  //Function: regexp-replace regexp string substitution 
  //Function: regexp-replace-all regexp string substitution 
  //Function: regexp-replace* string rx1 sub1 rx2 sub2 ... 
  //Function: regexp-replace-all* string rx1 sub1 rx2 sub2 ... 
  //Function: regexp-quote string 
  //Macro: rxmatch-let match-expr (var ...) form ... 
  //Macro: rxmatch-if match-expr (var ...) then-form else-form 
  //Macro: rxmatch-cond clause ... 
  //Macro: rxmatch-case string-expr clause ... 

}


//
// Library functions only work on Node.js
// see also: test/node_functions.js
//

(function(){
  if(BiwaScheme.on_node){
    var node = {
      fs: require('fs'),
      path: require('path'),
      process: process
    };
  }

  // Defines library functions which only works on Node.
  // - On Node: same as define_libfunc
  // - On Browser: defines a stub libfunc which just raises Error
  var define_node_libfunc = function(/*arguments*/){
    var args = _.toArray(arguments);

    if(BiwaScheme.on_node){
      BiwaScheme.define_libfunc.apply(null, args);
    }
    else{
      var func_name = args[0];
      var func = function(ar){
        throw new BiwaScheme.Error("the function '"+func_name+"' "+
          "is not supported in the browser "+
          "(works only on Node.js).");
      };
      args.pop();
      args.push(func);
      BiwaScheme.define_libfunc.apply(null, args);
    }
  };

  //
  // Chapter 9 File System
  //

  //(file-exists? filename)    procedure 
  define_node_libfunc("file-exists?", 1, 1, function(ar){
    assert_string(ar[0]);
    return node.path.existsSync(ar[0]);
  });

  //(delete-file filename)    procedure 
  define_node_libfunc("delete-file", 1, 1, function(ar){
    assert_string(ar[0]);
    node.fs.unlinkSync(ar[0]);
    return BiwaScheme.undef;
  });

  //
  // Chapter 10 Command-line access and exit values
  //
  
  //(command-line)    procedure
  define_node_libfunc("command-line", 0, 0, function(ar){
    return BiwaScheme.List.apply(null, node.process.argv);
  });

  //(exit)    procedure 
  //(exit obj)    procedure
  define_node_libfunc("exit", 0, 1, function(ar){
    var obj = ar[0];
    var code = _.isUndefined(obj) ? 0 :
               (obj === false)    ? 1 :
               Number(obj);

    node.process.exit(code);
  });

  //
  // srfi-98 (get-environment-variable)
  //

  // (get-environment-variable name) -> string or #f
  define_node_libfunc("get-environment-variable", 1, 1, function(ar){
    assert_string(ar[0]);
    var val = node.process.env[ar[0]];
    return _.isUndefined(val) ? false : val;
  });

  // (get-environment-variables) -> alist of string (("key" . "value")"
  define_node_libfunc("get-environment-variables", 0, 0, function(ar){
    return BiwaScheme.js_obj_to_alist(node.process.env);
  });

})();
//
// srfi.js - SRFI libraries
//
// should be src/library/srfi/1.js, etc (in the future).
//

with(BiwaScheme) {
  
  //
  // srfi-1 (list)
  //
  define_libfunc("iota", 1, 3, function(ar){
    var count = ar[0];
    var start = ar[1] || 0;
    var step = (ar[2]===undefined) ? 1 : ar[2];
    assert_integer(count);
    assert_number(start);
    assert_number(step);

    var a = [], n = start;
    for(var i=0; i<count; i++){
      a.push(n);
      n += step;
    }
    return array_to_list(a);
  });

  var copy_pair = function(pair){
    var car = BiwaScheme.isPair(pair.car) ? copy_pair(pair.car)
                                          : pair.car;
    var cdr = BiwaScheme.isPair(pair.cdr) ? copy_pair(pair.cdr)
                                          : pair.cdr;
    return new Pair(car, cdr);
  };
  // (list-copy list)
  define_libfunc("list-copy", 1, 1, function(ar){
    if(BiwaScheme.isPair(ar[0])){
      return copy_pair(ar[0]);
    }
    else{
      return BiwaScheme.nil;
    }
  });

  //
  // srfi-6 & Gauche (string port)
  // 
  define_libfunc("open-input-string", 1, 1, function(ar){
    assert_string(ar[0]);
    return new Port.StringInput(ar[0]);
  })
  define_libfunc("open-output-string", 0, 0, function(ar){
    return new Port.StringOutput();
  })
  define_libfunc("get-output-string", 1, 1, function(ar){
    assert_port(ar[0]);
    if(!(ar[0] instanceof Port.StringOutput))
      throw new Error("get-output-string: port must be made by 'open-output-string'");
    return ar[0].output_string();
  })

  //
  // srfi-8 (receive)
  //

  // (receive <formals> <expression> <body>...)
  // -> (call-with-values (lambda () expression)
  //                        (lambda formals body ...))
  define_syntax("receive", function(x){
    assert(BiwaScheme.isPair(x.cdr),
           "missing formals", "receive");
    var formals = x.cdr.car;
    assert(BiwaScheme.isPair(x.cdr.cdr),
           "missing expression", "receive");
    var expression = x.cdr.cdr.car;
    var body       = x.cdr.cdr.cdr;
    
    return deep_array_to_list([Sym("call-with-values"),
      [Sym("lambda"), BiwaScheme.nil, expression],
      new BiwaScheme.Pair(Sym("lambda"),
        new BiwaScheme.Pair(formals, body))]);
  });

  // srfi-19 (time)
  //
//  // constants
//time-duration
//time-monotonic
//time-process
//time-tai
//time-thread
//time-utc
  // Current time and clock resolution
  define_libfunc("current-date", 0, 1, function(ar){
    //todo: tz-offset (ar[1])
    return new Date();
  })
//
//current-julian-day -> jdn
//current-modified-julian-day -> mjdn
//current-time [time-type] -> time
//time-resolution [time-type] -> integer
//  // Time object and accessors
//make-time type nanosecond second -> time
//time? object -> boolean
//time-type time -> time-type
//time-nanosecond time -> integer
//time-second time -> integer
//set-time-type! time time-type
//set-time-nanosecond! time integer
//set-time-second! time integer
//copy-time time1 -> time2 
//  // Time comparison procedures
//time<=? time1 time2 -> boolean
//time<? time1 time2 -> boolean
//time=? time1 time2 -> boolean
//time>=? time1 time2 -> boolean
//time>? time1 time2 -> boolean
//  // Time arithmetic procedures
//time-difference time1 time2 -> time-duration
//time-difference! time1 time2 -> time-duration
//add-duration time1 time-duration -> time
//add-duration! time1 time-duration -> time
//subtract-duration time1 time-duration -> time
//subtract-duration! time1 time-duration -> time
  // Date object and accessors
  // make-date
  define_libfunc("date?", 1, 1, function(ar){
    return (ar[0] instanceof Date);
  })
  define_libfunc("date-nanosecond", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getMilliseconds() * 1000000;
  })
  define_libfunc("date-millisecond", 1, 1, function(ar){ // not srfi-19
    assert_date(ar[0]);
    return ar[0].getMilliseconds();
  })
  define_libfunc("date-second", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getSeconds();
  })
  define_libfunc("date-minute", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getMinutes();
  })
  define_libfunc("date-hour", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getHours();
  })
  define_libfunc("date-day", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getDate();
  })
  define_libfunc("date-month", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getMonth() + 1; //Jan = 0 in javascript..
  })
  define_libfunc("date-year", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getFullYear();
  })
  //date-zone-offset
  //date-year-day
  define_libfunc("date-week-day", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getDay();
  })
  //date-week-number

  // Time/Date/Julian Day/Modified Julian Day Converters
  // (snipped)
  
  // Date to String/String to Date Converters
  // TODO: support locale
  //   * date_names
  //   * ~f 5.2 sec
  //   * ~p AM/PM
  //   * ~X 2007/01/01
  BiwaScheme.date_names = {
    weekday: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
    full_weekday: ["Sunday", "Monday", "Tuesday", 
      "Wednesday", "Thursday", "Friday", "Saturday"],
    month: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    full_month: ["January", "February", "March", "April",
      "May", "June", "July", "August", "September", 
      "Octorber", "November", "December"]
  }

  BiwaScheme.date2string = function(date, format){
    var zeropad  = function(n){ return n<10 ? "0"+n : ""+n }; 
    var spacepad = function(n){ return n<10 ? " "+n : ""+n }; 
    
    var getter = {
      a: function(x){ return date_names.weekday[x.getDay()] },
      A: function(x){ return date_names.full_weekday[x.getDay()] },
      b: function(x){ return date_names.month[x.getMonth()] },
      B: function(x){ return date_names.full_month[x.getMonth()] },
      c: function(x){ return x.toString() },
      d: function(x){ return zeropad(x.getDate()) },
      D: function(x){ return getter.d(x) + getter.m(x) + getter.y(x); },
      e: function(x){ return spacepad(x.getDate()) },
      f: function(x){ return x.getSeconds() + x.getMilliseconds()/1000; },
      h: function(x){ return date_names.month[x.getMonth()] },
      H: function(x){ return zeropad(x.getHours()) },
      I: function(x){ var h = x.getHours(); return zeropad(h<13 ? h : h-12) },
      j: function(x){ throw new Bug("not implemented: day of year") },
      k: function(x){ return spacepad(x.getHours()) },
      l: function(x){ var h = x.getHours(); return spacepad(h<13 ? h : h-12) },
      m: function(x){ return zeropad(x.getMonth()) },
      M: function(x){ return zeropad(x.getMinutes()) },
      n: function(x){ return "\n" },
      N: function(x){ throw new Bug("not implemented: nanoseconds") },
      p: function(x){ return x.getHours()<13 ? "AM" : "PM" },
      r: function(x){ return getter.I(x) + ":" + getter.M(x) + ":" + getter.S(x) + " " + getter.p(x) },
      s: function(x){ return Math.floor(x.getTime() / 1000) },
      S: function(x){ return zeropad(x.getSeconds()) },
      t: function(x){ return "\t" },
      T: function(x){ return getter.H(x) + ":" + getter.M(x) + ":" + getter.S(x) },
      U: function(x){ throw new Bug("not implemented: weeknum(0~, Sun)") },
      V: function(x){ throw new Bug("not implemented: weeknum(1~, Sun?)") },
      w: function(x){ return x.getDay() },
      W: function(x){ throw new Bug("not implemented: weeknum(0~, Mon)") },
      x: function(x){ throw new Bug("not implemented: weeknum(1~, Mon)") },
      X: function(x){ return getter.Y(x) + "/" + getter.m(x) + "/" + getter.d(x) },
      y: function(x){ return x.getFullYear() % 100 },
      Y: function(x){ return x.getFullYear() },
      z: function(x){ throw new Bug("not implemented: time-zone") },
      Z: function(x){ throw new Bug("not implemented: symbol time zone") },
      1: function(x){ throw new Bug("not implemented: ISO-8601 year-month-day format") },
      2: function(x){ throw new Bug("not implemented: ISO-8601 hour-minute-second-timezone format") },
      3: function(x){ throw new Bug("not implemented: ISO-8601 hour-minute-second format") },
      4: function(x){ throw new Bug("not implemented: ISO-8601 year-month-day-hour-minute-second-timezone format") },
      5: function(x){ throw new Bug("not implemented: ISO-8601 year-month-day-hour-minute-second format") }
    }

    return format.replace(/~([\w1-5~])/g, function(str, x){
      var func = getter[x];
      if(func)
        return func(date);
      else if(x == "~")
        return "~";
      else
        return x;
    })
  }
  
  // date->string
  define_libfunc("date->string", 1, 2, function(ar){
    assert_date(ar[0]);

    if(ar[1]){
      assert_string(ar[1]);
      return date2string(ar[0], ar[1]);
    }
    else
      return ar[0].toString();
  })
  // string->date

  define_libfunc("parse-date", 1, 1, function(ar){ // not srfi-19
    assert_string(ar[0]);
    return new Date(Date.parse(ar[0]));
  })

  //
  // srfi-27
  //
  define_libfunc("random-integer", 1, 1, function(ar){
    var n = ar[0];
    assert_integer(n);
    if (n < 0)
      throw new Error("random-integer: the argument must be >= 0");
    else
      return Math.floor(Math.random() * ar[0]);
  });
  define_libfunc("random-real", 0, 0, function(ar){
    return Math.random();
  });

  //
  // srfi-28 (format)
  //

  // (format format-str obj1 obj2 ...) -> string
  // (format #f format-str ...) -> string
  // (format #t format-str ...) -> output to current port 
  // (format port format-str ...) -> output to the port 
  define_libfunc("format", 1, null, function(ar){
    if (_.isString(ar[0])) {
      var port = null, format_str = ar.shift();
    }
    else if (ar[0] === false) {
      ar.shift();
      var port = null, format_str = ar.shift();
    }
    else if (ar[0] === true) {
      ar.shift();
      var port = BiwaScheme.Port.current_output,
          format_str = ar.shift();
    }
    else {
      var port = ar.shift(), format_str = ar.shift();
      assert_port(port);
    }

    var str = format_str.replace(/~[as]/g, function(matched){
                 assert(ar.length > 0,
                        "insufficient number of arguments", "format");
                 if (matched == "~a")
                   return BiwaScheme.to_display(ar.shift());
                 else
                   return BiwaScheme.to_write(ar.shift());
              }).replace(/~%/, "\n")
                .replace(/~~/, "~");
    if (port) {
      port.put_string(str);
      return BiwaScheme.undef;
    }
    else {
      return str;
    }
  });
  
  //
  // srfi-38 (write/ss)
  //
  var user_write_ss = function(ar){
    puts(write_ss(ar[0]), true);
    return BiwaScheme.undef;
  }
  define_libfunc("write/ss", 1, 2, user_write_ss);
  define_libfunc("write-with-shared-structure", 1, 2, user_write_ss);
  define_libfunc("write*", 1, 2, user_write_ss); //from Gauche(STklos)

  //
  // srfi-43 (vector library)
  //
  define_libfunc("vector-append", 2, null, function(ar){
    var vec = [];
    return vec.concat.apply(vec, ar);
  });

  // (vector-copy vector)
  define_libfunc("vector-copy", 1, 1, function(ar){
    assert_vector(ar[0]);
    return _.clone(ar[0]);
  });

  //
  // see src/library/node_functions.js for:
  // - srfi-98 (get-environment-variable)
  //
}
BiwaScheme.run = function(code) {
  var intp = new BiwaScheme.Interpreter(function(e){
    if(e.stack){
      require('util').puts(e.stack);
    }
    else{
      require('util').puts(e);
    }

    throw e;
  });
  return intp.evaluate(code);
};

BiwaScheme.run_file = function(filename, encoding/*optional*/) {
  var enc = encoding || 'utf8';
  var src = require('fs').readFileSync(filename, enc);
  return BiwaScheme.run(src);
};

BiwaScheme.define_libfunc("load", 1, 1, function(ar) {
  var relpath = ar[0];
  BiwaScheme.assert_string(relpath);
  // assume path is relative to node_modules directory
  var filename = __dirname + "/../../../" + relpath;
  var code = require("fs").readFileSync(filename, "utf8");
  return BiwaScheme.run(code);
});

BiwaScheme.define_libfunc("js-load", 1, 1, function(ar) {
  var relpath = ar[0];
  BiwaScheme.assert_string(relpath);
  // assume path is relative to node_modules directory
  return require(__dirname + "/../../../" + relpath);
});

for(x in BiwaScheme){
  exports[x] = BiwaScheme[x];
}
