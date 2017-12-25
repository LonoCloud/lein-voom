FROM ubuntu:17.10

RUN apt-get update
RUN apt-get install -y openjdk-8-jre curl git

RUN curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > /bin/lein
RUN chmod 755 /bin/lein

RUN curl https://raw.githubusercontent.com/LonoCloud/lein-voom/master/bin/box > /bin/box
RUN chmod 755 /bin/box

ARG LEIN_VER=SET_ME
ENV LEIN_VER=$LEIN_VER

RUN sed -i 's/\(export LEIN_VERSION\)="[^"]*"/\1="'$LEIN_VER'"/' /bin/lein

ARG VOOM_VER=SET_ME
ENV VOOM_VER=$VOOM_VER

RUN mkdir -p ~/.lein
RUN echo '{:system {:plugins [[lein-voom "'${VOOM_VER}'"]]}}' > ~/.lein/profiles.clj

# If available, pre-fetched or locally built lein-voom images will be copied in by test script.
COPY artifacts /root/.m2/repository

# Bootstrap lein install
RUN lein version

