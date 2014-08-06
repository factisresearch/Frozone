FROM agrafix/ghc7.6
MAINTAINER Alexander Thiemann <thiemann@cp-med.com>

ENV DEBIAN_FRONTEND noninteractive
RUN locale-gen en_US.UTF-8
ENV LC_ALL en_US.UTF-8

RUN apt-get -qq update
RUN apt-get upgrade -y

RUN apt-get install -y software-properties-common libsasl2-modules bsd-mailx
RUN add-apt-repository -y ppa:chris-lea/node.js
RUN apt-get -qq update
RUN apt-get install -y nodejs
RUN npm install -g react-tools

RUN cabal update

# Install happy
RUN wget -q http://hackage.haskell.org/package/happy-1.19.3/happy-1.19.3.tar.gz
RUN tar xzvf happy-1.19.3.tar.gz
RUN cd happy-1.19.3 && cabal install
RUN rm -rf happy-1.19.3*

# Install alex
RUN wget -q http://hackage.haskell.org/package/alex-3.1.3/alex-3.1.3.tar.gz
RUN tar xzvf alex-3.1.3.tar.gz
RUN cd alex-3.1.3 && cabal install
RUN rm -rf alex-3.1.3*

# Install Frozone
RUN git clone https://github.com/factisresearch/Frozone.git
WORKDIR /Frozone
RUN cabal sandbox init
RUN chmod +x ./build-helper.sh
RUN ./build-helper.sh
RUN cabal sandbox add-source darcs-2.8.5
RUN cabal -j12 install --only-dependencies

# Configure & build Frozone
RUN cabal configure
RUN cabal build

# Compile jsx
RUN jsx jssrc/ static/js/out

# Copy to new directory
RUN mkdir -p /frozone-bin
RUN cp dist/build/Frozone/Frozone /frozone-bin/Frozone
RUN cp -r static/ /frozone-bin/static
RUN cp config.yml /frozone-bin/config.yml
RUN chmod +x /frozone-bin/Frozone

WORKDIR /frozone-bin
RUN mkdir -p scratch/storage
RUN rm -rf /Frozone

# configure sendmail
RUN apt-get install -q -y sendmail

# supervisor
RUN apt-get install -y supervisor
RUN mkdir -p /var/log/frozone

ADD supervisord.conf /etc/supervisor/conf.d/supervisord.conf
CMD ["/usr/bin/supervisord"]
