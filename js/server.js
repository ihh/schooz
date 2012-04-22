var BiwaScheme = require("biwascheme");
BiwaScheme.run_file("scm/core/schooz.scm");
BiwaScheme.run_file("scm/core/machines.scm");
BiwaScheme.run_file("scm/ui/terminal.scm");
BiwaScheme.run_file("scm/api/schooz.scm");
BiwaScheme.run_file("scm/api/machines.scm");
BiwaScheme.run_file("scm/demo/rock.scm");
BiwaScheme.run("(main-loop)");
