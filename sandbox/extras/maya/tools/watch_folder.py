from xml.dom.minidom import parseString
import os
import sys
import time
from datetime import datetime
import shutil


CLI_PATH = '/projects/appleseed/sandbox/bin/Release/appleseed.cli'
FLAGS = ''

print '\n\n\n'

#define helper class for printing colored text
class printc():
	@staticmethod
	def warning(text):
		if os.system == 'darwin':
			print '\033[93m' + text + '\033[0m'
		else: 
			print text
	
	@staticmethod
	def error(text):
		if os.system == 'darwin':
			print '\033[91m' + text + '\033[0m'
		else: 
			print text
			
	@staticmethod
	def success(text):
		if os.system == 'darwin':
			print '\033[92m' + text + '\033[0m'
		else: 
			print text



def getDepends(xml_file_path):

	depend_list = []

	file = open(xml_file_path,'r')
	directory = os.path.split(xml_file_path)[0]
	data = file.read()
	file.close()

	dom = parseString(data)

	for entity in dom.getElementsByTagName('parameter'):
		if entity.getAttribute('name') == 'filename':

			file_name_attr = entity.getAttribute('value')


			if (sys.platform == 'win32') or (sys.platform == 'win64'):
				file_name_attr = file_name_attr.replace('/', '\\')
			else:
				file_name_attr = file_name_attr.replace('\\', '/')

			depend_list.append( os.path.join( directory,  file_name_attr) )




	return depend_list


def listAppleseedFiles(directory_path):
	directory_entities =  os.listdir(directory_path)
	files = []
	appleseed_files = []
	for entity in directory_entities:
		file_path = os.path.join(directory_path, entity)
		if os.path.isfile(file_path):
			if os.path.splitext(file_path)[1] == '.appleseed':
				appleseed_files.append(file_path)



	return appleseed_files


def isRenderable(file):
	print '\n'
	depend_name_text = ('Dependencies for ' + os.path.split(file)[1])
	print depend_name_text
	print len(depend_name_text) * '-'
	is_renderable = True
	for depend in getDepends(file):
		if os.path.exists(os.path.join(depend)):
			printc.success('EXISTS   ' + depend)
		else:
			printc.error('MISSING  ' + depend)
			is_renderable = False
	return is_renderable




def main():
    
    working_dir = os.getcwd()

    # make folder to put rendered appleseed files into
    if not os.path.exists(os.path.join(working_dir, 'done')):
    	os.mkdir(os.path.join(working_dir, 'done'))

    # make folder to put rendered images into
    if not os.path.exists(os.path.join(working_dir, 'output')):
    	os.mkdir(os.path.join(working_dir, 'output'))

    while (True):
		appleseed_files = listAppleseedFiles(working_dir)

		#if any appleseed files have been found
		if len(appleseed_files):
			for appleseed_file in appleseed_files:
				if isRenderable(appleseed_file):


					printc.warning('\n\n:::: RENDERING ' + appleseed_file + ' ::::\n\n')

					#create shell command
					appleseed_file_name = os.path.split(appleseed_file)[1]
					output_file_name = os.path.splitext(appleseed_file_name)[0] + '.png'
					output_file_path = os.path.join(working_dir, 'output', output_file_name)

					command = CLI_PATH + ' -o ' + output_file_path + ' ' + appleseed_file
					
					#execute command
					return_value = os.system(command)

					#if the return value isnt 0 then somehtign may have gone wrong
					if not return_value == 0:
						printc.warning('File may not have rendered correctly: ' + appleseed_file)

					move_dest = os.path.join(working_dir, 'done', os.path.split(appleseed_file)[1])
					shutil.move(appleseed_file,move_dest)

					break
				else:
					print '\n', datetime.now(), os.path.split(appleseed_file)[1], ': Missing dependencies'
		else:
			print '\n', datetime.now(), ': Nothing to render' 
		time.sleep(3)


if __name__ == '__main__':
    main()
















