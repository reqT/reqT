/***
**                  _______
**                 |__   __|   reqT - a requirements engineering tool
**   _ __  ___   __ _ | |      (c) 2010-2017, Lund University
**  |  __|/ _ \ / _  || |      http://reqT.org
**  | |  |  __/| (_| || |
**  |_|   \___| \__  ||_|
**                 | |
**                 |_|
** reqT is open source, licensed under the BSD 2-clause license:
** http://opensource.org/licenses/bsd-license.php
**************************************************************************/

package reqT

trait GlobalConstants { //mixed in by package object reqT
  val reqT_VERSION = "3.1.3"
  val reqT_BUILD = 565
  val SCALA_VERSION = scala.util.Properties.versionString
  lazy val PREAMBLE = """
**                  _______
**                 |__   __|
**   _ __  ___   __ _ | |      reqT - a requirements engineering tool
**  |  __|/ _ \ / _  || |      http://reqT.org
**  | |  |  __/| (_| || |      (c) 2010-2019, Lund University
**  |_|   \___| \__  ||_|      Open Source BSD-2-clause license
**                 | |
**                 |_|
"""

  lazy val LICENCE = """
http://reqT.org/license
=======================

ReqT is open source under the BSD 2-clause license:
http://opensource.org/licenses/bsd-license.php

Copyright (c) 2010-2019 Lund University, Sweden.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

"""

}