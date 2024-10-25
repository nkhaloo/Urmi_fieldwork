# This script goes through sound and TextGrid files in a directory,
# opens each pair of Sound and TextGrid, calculates
# ***the mean formant values*** (modified from ***the formant values at the midpoint***)
# of each labeled interval, and saves results to a text file.
# To make some other or additional analyses, you can modify the script
# yourself... it should be reasonably well commented! ;)
#
# This script is distributed under the GNU General Public License.
# Copyright 4.7.2003 Mietta Lennes
# 
# ***Modified on 10.23.2013 by Yu Tanaka for UCLA Ling 104 Fall 2013 and a few tweaks for later courses.***
# 

form Analyze formant values from labeled segments in files
	comment Directory of sound files
	text sound_directory /Users/ritalavi/Desktop/Urmi_fieldwork/data/soundfiles/
	sentence Sound_file_extension .wav
	comment Directory of TextGrid files
	text textGrid_directory /Users/ritalavi/Desktop/Urmi_fieldwork/data/soundfiles/
	sentence TextGrid_file_extension .TextGrid
	comment Full path of the resulting text file:
	text resultfile /Users/ritalavi/Desktop/results.csv
	comment Which tier do you want to analyze?
	sentence Tier vowels
	comment Formant analysis parameters
	positive Time_step 0.01
	positive Maximum_number_of_formants 5.0
	#modified from 4, and now not an integer
	positive Maximum_formant_(Hz) 5000
	positive Window_length_(s) 0.025
	real Preemphasis_from_(Hz) 50
endform

# Here, you make a listing of all the sound files in a directory.
# The example gets file names ending with ".wav" from D:\tmp\

Create Strings as file list... list 'sound_directory$'*'sound_file_extension$'
numberOfFiles = Get number of strings

# Check if the result file exists:
if fileReadable (resultfile$)
	pause The result file 'resultfile$' already exists! Do you want to overwrite it?
	filedelete 'resultfile$'
endif

# Write a row with column titles to the result file:
# (remember to edit this if you add or change the analyses!)

titleline$ = "Filename,Segment,F1,F2'newline$'"
fileappend "'resultfile$'" 'titleline$'

# Go through all the sound files, one by one:

for ifile to numberOfFiles
	filename$ = Get string... ifile
	# A sound file is opened from the listing:
	Read from file... 'sound_directory$''filename$'
	# Starting from here, you can add everything that should be 
	# repeated for every sound file that was opened:
	soundname$ = selected$ ("Sound", 1)
	To Formant (burg)... time_step maximum_number_of_formants maximum_formant window_length preemphasis_from
	# Open a TextGrid by the same name:
	gridfile$ = "'textGrid_directory$''soundname$''textGrid_file_extension$'"
	if fileReadable (gridfile$)
		Read from file... 'gridfile$'
		# Find the tier number that has the label given in the form:
		call GetTier 'tier$' tier
		numberOfIntervals = Get number of intervals... tier
		# Pass through all intervals in the selected tier:
		for interval to numberOfIntervals
			label$ = Get label of interval... tier interval
			if label$ <> ""
				# if the interval has an unempty label, get its start and end:
				start = Get starting point... tier interval
				end = Get end point... tier interval
				#midpoint = (start + end) / 2
				# get the formant values at that interval
				select Formant 'soundname$'
				f1 = Get mean... 1 start end Hertz
				f2 = Get mean... 2 start end Hertz
				#f3 = Get mean... 3 start end Hertz	#no f3
				# Save result to text file:
				resultline$ = "'soundname$','label$','f1','f2''newline$'"
				fileappend "'resultfile$'" 'resultline$'
				select TextGrid 'soundname$'
			endif
		endfor
		# Remove the TextGrid object from the object list
		select TextGrid 'soundname$'
		Remove
	endif
	# Remove the temporary objects from the object list
	select Sound 'soundname$'
	plus Formant 'soundname$'
	Remove
	select Strings list
	# and go on with the next sound file!
endfor

Remove


#-------------
# This procedure finds the number of a tier that has a given label.

procedure GetTier name$ variable$
        numberOfTiers = Get number of tiers
        itier = 1
        repeat
                tier$ = Get tier name... itier
                itier = itier + 1
        until tier$ = name$ or itier > numberOfTiers
        if tier$ <> name$
                'variable$' = 0
        else
                'variable$' = itier - 1
        endif

	if 'variable$' = 0
		exit The tier called 'name$' is missing from the file 'soundname$'!
	endif

endproc