# Game of Life - PNG Image Generator

ImageMagick, libpng, and libpng headers must be installed to build this project.  
To install these packages on Ubuntu: 

    sudo apt-get install imagemagick libpng-dev

To create PNG outputs and convert to an animation, run:

    make && ./life && ./convert.sh

<img class="doc" src="../../docs/images/game-of-life-gliders.gif">
