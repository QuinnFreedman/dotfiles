#!/bin/bash

SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )" 
mkdir -p ~/.config/nvim && \
ln -s $SCRIPTPATH/.config/nvim/init.vim ~/.config/nvim/init.vim && \
cp -r $SCRIPTPATH/.config/nvim/autoload ~/.config/nvim/
pip3 install neovim
pip2 install neovim
