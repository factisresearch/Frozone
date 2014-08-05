FROM agrafix/ghc7.6
MAINTAINER Alexander Thiemann <thiemann@cp-med.com>

RUN add-apt-repository -y ppa:chris-lea/node.js
RUN apt-get -qq update
RUN apt-get install -y nodejs
RUN npm install -g react-tools

RUN cabal update

RUN git clone https://github.com/factisresearch/Frozone.git
WORKDIR /Frozone
RUN chmod +x ./build-helper.sh
RUN ./build-helper.sh
RUN cd darcs-2.8.5 && cabal install
RUN cabal install --only-dependencies
