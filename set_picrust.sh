#! /bin/bash

#Install conda first

cd python

wget https://github.com/picrust/picrust2/archive/refs/tags/v2.3.0-b.tar.gz
tar xvzf  v2.3.0-b.tar.gz
cd picrust2-2.3.0-b
conda env create -f  picrust2-env.yaml
conda activate picrust2
pip install --editable .

cd ..