from ubuntu:latest

MAINTAINER justin.ethier@gmail.com

ARG DEBIAN_FRONTEND=noninteractive

ENV CYCLONE_VERSION v0.23
RUN apt update -y
RUN apt install -y build-essential git rsync texinfo libtommath-dev libck-dev make gcc

RUN git clone https://github.com/justinethier/cyclone-bootstrap && cd cyclone-bootstrap && git fetch -a && git checkout ${CYCLONE_VERSION} && make && make install

CMD ["icyc"]
