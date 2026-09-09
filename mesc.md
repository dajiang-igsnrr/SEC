---
project: MESC
summary: A Fortran-based microbial-explicit soil carbon cycle model
author: CALIPSO-MESC Development Team
src_dir: src/
output_dir: doc/
exclude_dir: **/build*
project_github: https://github.com/CALIPSO-MESC/MESC
license: gpl
sort: alpha
source: true
graph: true
externalize: true
md_extensions: markdown.extensions.toc
               markdown.extensions.tables
               markdown.extensions.fenced_code
---

--------------------

[TOC]

Brief description
-----------------

The MESC model is a process-oriented soil carbon model that explicitly
represents microbial decomposition processes using Michaelis-Menten
kinetics, designed to simulate soil and ecosystem carbon dynamics and
their controlling mechanisms. It supports a combined MIMICS-MILLENNIAL2
kinetic approach, bioturbation via Crank-Nicolson discretization, and
can be forced by CABLE or ORCHIDEE outputs.

License
-------

MESC is licensed under the GNU General Public License v3.0. See
[LICENSE](LICENSE) for details.
