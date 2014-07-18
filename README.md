[![Build Status](https://travis-ci.org/factisresearch/Frozone.svg)](https://travis-ci.org/factisresearch/Frozone)

Frozone
=====

We're building Frozone to replace our Jenkins Continuous Integration system.  Frozone will enable a team of developers to check source code changes by compiling the project and running the unit tests before the changes get commited to a collaboration repository.  Frozone will be enable to automatically deploy and run those changes for review by other developers and the QA team.  The reviewers will be able to review the changes and accept or reject them.

Using docker the developers can fully describe the system needed to run a program including the distribution (Ubuntu 14.04), packages that need to be installed (apt-get install nodejs), the libraries that are required (npm install bloomfilter) and all needed configuration.  Even though a clean build of the complete system might take an hour Frozone will be able to give feedback about your changes much faster by using an intelligent cache that depends on the build description.  For our project we're aiming to get feedback for most cases in less than 2 minutes.

Frozone is far from aplha quality and heavily under development.  This is a description of what it will look like when we reach alpha quality.  We're fast coders so stay tuned!

# Install

* From Source: `git clone https://github.com/factisresearch/Frozone.git && cd Frozone && cabal install`

# Todo

* Better README
* Usage examples
* Continous deployment
* Dockerize
* Hooks (Post-success, etc.)
