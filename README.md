[![Build Status](https://travis-ci.org/factisresearch/Frozone.svg)](https://travis-ci.org/factisresearch/Frozone)

Frozone
=====

We're building Frozone to replace our Jenkins Continuous Integration system.  Frozone will enable a team of developers to check source code changes by compiling the project and running the unit tests before the changes get commited to a collaboration repository.  Frozone will be enable to automatically deploy and run those changes for review by other developers and the QA team.  The reviewers will be able to review the changes and accept or reject them.

Using docker the developers can fully describe the system needed to run a program including the distribution (Ubuntu 14.04), packages that need to be installed (apt-get install nodejs), the libraries that are required (npm install bloomfilter) and all needed configuration.  Even though a clean build of the complete system might take an hour Frozone will be able to give feedback about your changes much faster by using an intelligent cache that depends on the build description.  For our project we're aiming to get feedback for most cases in less than 2 minutes.

Frozone is far from alpha quality and heavily under development.  This is a description of what it will look like when we reach alpha quality.  We're fast coders so stay tuned!

# Install

* From Source:
Check the Dockerfile located at ./docker/Dockerfile

* Docker:
```bash
git clone https://github.com/factisresearch/Frozone.git
cd Frozone
docker build --rm -t [your_name_here]/frozone docker
```

# Run

* Docker
```bash
docker run -v /var/run/docker.sock:/var/run/docker.sock -v $(which docker):$(which docker) -d -p 8080:8080 [your_name_here]/frozone
```

* Without docker
```bash
./Frozone [config_file]
```

# Usage

In your development repository, you'll need a `.frozone.yml` where you define your cook-file directory, your entry point and boring file. Example:

```yml
cookDir: server/cook
entryPoint: 07_fullbuild.cook
boringFile: .boring
```
Explanation:

* cookDir: the path where the two other files are located
* entryPoint: the cook file, describing how to build the project
* boringFile: used by docker(cook) ?

`.frozone.yml` has to be part of the repository of your project, as well as the files referenced by it!

(For more information on this, see [dockercook](https://github.com/factisresearch/dockercook))

To upload a darcs patch from your repository to Frozone, this script will help you:

./send_patch PROJECT-SHORTNAME

Send patches to Frozone by calling this script.

# Todo

* Continous deployment
* Hooks (Post-success, etc.)
