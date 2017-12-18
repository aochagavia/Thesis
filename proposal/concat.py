# Concat files in `chapters`
chapterDir = 'chapters'
chapters = [
    'title.md',
    'introduction.md',
    'background.md',
    'preliminary_results.md',
    'timetable.md'
]

with open('bundle.md', 'w') as outfile:
    for fname in chapters:
        filePath = '{}/{}'.format(chapterDir, fname)
        with open(filePath) as infile:
            outfile.write(infile.read())
            outfile.write('\n') # Extra new line after each file

    # Add a chapter for the references
    outfile.write('# References\n\n')