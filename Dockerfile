FROM rocker/shiny:4.3
ENV PATH="/root/miniconda3/bin:${PATH}"
ARG PATH="/root/miniconda3/bin:${PATH}"
# Install system requirements
RUN apt-get update && apt-get install -y wget \
    libglpk-dev \
    pandoc \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-latex-extra

RUN wget \
    https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
    && mkdir /root/.conda \
    && bash Miniconda3-latest-Linux-x86_64.sh -b \
    && rm -f Miniconda3-latest-Linux-x86_64.sh 
RUN conda --version


RUN mkdir /app
RUN mkdir /app/samba_files
RUN mkdir /app/samba_data

COPY ./container_build/dep_check_1.R /app/
RUN Rscript /app/dep_check_1.R 

COPY ./python /app/python
WORKDIR /app/python

RUN wget https://github.com/picrust/picrust2/archive/refs/tags/v2.3.0-b.tar.gz
RUN tar xvzf  v2.3.0-b.tar.gz
WORKDIR /app/python/picrust2-2.3.0-b
RUN conda env create -f  picrust2-env.yaml
RUN conda init bash
#RUN echo "conda activate picrust2" >> ~/.bashrc
#SHELL ["/bin/bash", "--login", "-c"]
#RUN conda activate picrust2
RUN pip install --editable .

WORKDIR /




COPY ./ /app/samba
COPY container_build/configs.R /app/samba/configs.R

# USER 1001

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app/samba',port = 3838, host = '0.0.0.0')"]
