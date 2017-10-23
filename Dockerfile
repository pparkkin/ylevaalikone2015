FROM apache/zeppelin:0.7.3

MAINTAINER pparkkin@gmail.com

RUN conda list -n root --export | grep -Ev '^(conda|ipaddress|enum34)' | awk -F'=' '{print $1}' > /tmp/conda-root-package-list.txt
RUN conda create --name python35 python=3.5
RUN conda install -n python35 --file /tmp/conda-root-package-list.txt
RUN conda install -n python35 matplotlib
RUN conda install -n python35 pandas
