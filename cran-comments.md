# Round 2 (Resubmission)

2015-10-22 8:29 GMT+02:00 Kurt Hornik <Kurt.Hornik@wu.ac.at>:

You mis-use file LICENSE: for MIT this should only be the completed template, see <https://www.r-project.org/Licenses/MIT>.

* **Updated the LICENSE file following the guidelines**

--------

Possibly mis-spelled words in DESCRIPTION:
  UptimeRobot (4:19)
  Uptimerobot (5:51)

Pls be consistent, and use the Description to explain the service some
more. 

* **Fixed for consistence and added a more verbose description**

-----------

Pls also provide a URL for the service, as
See <http://.....> for more information.

* **Done**

---------

Finally,

Undefined global functions or variables:
  URLencode
Consider adding
  importFrom("utils", "URLencode")
to your NAMESPACE.

Pls fix

* **Fixed the mispelled @import clauses**

--------------

Best
-k

# Round 1

## Test environments
* ubuntu 12.04 (on travis-ci), R 3.2.2
* Windows 7 (local),  Revolution R Open 3.2.2

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE  
Maintainer: 'Gabriele Baldassarre <gabriele@gabrielebaldassarre.com>'
New submission

License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  The MIT License (MIT)
  
  Copyright (c) 2015 Gabriele Baldassarre
  
  [...CUT...]
