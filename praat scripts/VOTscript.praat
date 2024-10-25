form Open all files in a directory
# The command `form' opens a type of pop-up menu called a user dialog form
# The text `Open all files in a directory'  appears in the caption at the very top of the pop-up menu'
# This caption appears at the very top of the window, along with the default close button

sentence Directory /Users/ritalavi/Desktop/Urmi_fieldwork/data/stops/VOT_textgrids/
# sentence Directory

# The command `sentence' prints an input text window with left-hand label
# The text `Directory' appears as the left-hand label to the text box
# The text `YOUR DIRECTORY HERE' appears in the text box
# YOU MUST replace this text with the directory that contains your Kwak'wala wav files and textgrids

endform
# End of the user dialog form
# When you close the form, there will be a variable named `directory$' that contains the
# text in the text window that you left when you clicked `OK'.

Create Strings as file list... list 'directory$'
# The command `Create Strings as file list...' appears in the `New' menu in the PRAAT object browser
# The text `list' becomes the name of `Strings' object created.
# Do NOT confuse a `Strings' object with primitive `string' data.  An object is massively complicated
# compared to any of the primitives (`string', and `numeric' data).
# At its core, `Strings' is a list of individual `string' primitives

numberOfFiles = Get number of strings
# The command `Get number of strings' retrieves the count of the `string' primitives stored in 
# a recently selected `Strings' object - in this case, `list'

filedelete results.csv
fileappend results.csv Word,VOT 

for ifile to numberOfFiles
# Creates a loop that counts from 1 to `numberOfFiles', which was earlier derived from the 
# Number of files in the directory

	filename$ = Get string... ifile
	# takes a `string' primitive from the recently selected `Strings' object corresponding to
	# the value of the numeric `ifile'

		# You can add some filename extensions that you want to be included to the next line.
		if right$ (filename$, 4) = ".wav"

			Read from file... 'directory$''filename$'
			# reads the files in to the PRAAT object window by adding the file name
			# to the directory path

		elsif right$ (filename$, 9) = ".TextGrid"

			Read from file... 'directory$''filename$'

			textgridname$ = left$ (filename$,length(filename$)-9)
			# extract the name of the textgrid by removing the extension `.TextGrid'
			select TextGrid 'textgridname$'
			numberOfPoints = Get number of points... 2
			initvoice = 0
			endvoice = 0

			for i from 1 to numberOfPoints
				# select the TextGrid from the object menu - only works if it has a unique name
				label$ = Get label of point... 2 i
				# obtains the first label from the first *point* tier
				time = Get time of point... 2 i
				interval = Get interval at time... 1 time
				word$ = Get label of interval... 1 interval

				if label$ = "release"			
				# confirms the label is named "release" - script *crashes* later if the label is misnamed
			   		release = Get time of point... 2 i
				elsif label$ = "voicing"
				# confirms the label is named "voicing" - script *crashes* later if the label is misnamed
			   		voicing = Get time of point... 2 i
				elsif label$ = "release + voicing"
			   		release = 0
			   		voicing = 0
				endif
			endfor

			vot = voicing - release
			# calculates a variable named 'vot' as the voicing time minus release time

			fileappend results.csv 'newline$''textgridname$','vot:4'
			# prints a new line (`newline$') word, vot.
		endif
	
	select Strings list
	# This command is needed because the `Get' command changes the selected object
	# and you need to change it back to the `Strings' object named `list'

endfor

select Strings list
Remove
# removes the `Strings' object named `list' from the selection list