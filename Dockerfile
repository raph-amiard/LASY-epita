FROM ubuntu:latest

RUN apt update
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC apt-get -y install tzdata
RUN apt install texlive-xetex -y
RUN apt install make -y
RUN apt install pandoc -y

WORKDIR /workspace

ENTRYPOINT make pdf
