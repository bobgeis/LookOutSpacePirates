"""
This is used to make star field backgrounds.
"""


import sys, random, pygame, glob, fnmatch
from pygame.locals import *


# star colors
#     spectral type	   R   G   B
SPECTRA = {	'O': 	(225,225,255), \
			'B':	(225,255,255), \
			'A':	(255,255,255), \
			'F':	(255,255,225), \
			'G':	(255,255,200), \
			'K':	(255,225,200), \
			'M':	(255,200,200)}


STARS = { 'g' : {} , 'd' : {} }

def replace_color(color1, color2, img):
	""" replace color1 with color2 in img """
	img = img.copy()
	pixObj = pygame.PixelArray(img)
	img_size = img.get_size()
	for x in range(img_size[0]):
		for y in range(img_size[1]):
			if pixObj[x][y] == img.map_rgb(color1):
				pixObj[x][y] = color2
	del pixObj	
	return img


def load_stars():
	"""Load stars and create colored star images in the global STARS dict"""
	
	img = pygame.image.load('./images/dGrey.png')
	for type in SPECTRA:
		new_img = replace_color((150,150,150), SPECTRA[type], img)
		STARS['d'][type] = new_img

	img = pygame.image.load('./images/gGrey.png')
	for type in SPECTRA:
		new_img = replace_color((150,150,150), SPECTRA[type], img)
		STARS['g'][type] = new_img





def main():
	
	sizes = STARS.keys()
	colors = SPECTRA.keys()
	
	side = 6400
	
	load_stars()
	bg = pygame.Surface((side,side),SRCALPHA)
	
	for i in range(10000):
		size = random.choice(sizes)
		color = random.choice(colors)
		x = random.randint(0,side)
		y = random.randint(0,side)
		star = STARS[size][color]
		bg.blit(star, [x,y])
	pygame.image.save(bg, './images/test2.png')


if __name__ == '__main__':
	main()