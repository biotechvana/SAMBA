#! /bin/bash

#Install conda first

cd /opt/
wget https://github.com/picrust/picrust2/archive/v2.4.1.tar.gz
tar xvzf  v2.4.1.tar.gz
cd picrust2-2.4.1/

conda env create -f  picrust2-env.yaml
conda activate picrust2
pip install --editable .