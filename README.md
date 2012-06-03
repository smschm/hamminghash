hamminghash
===========

### Stuffs for making entropy user-friendly.

This program can do two things:

- Take a source text corpus and turn it into a *set* of prefix-free codes for a letter, one for each possible group of N consecutive letters (for variable N).

- Take a set of codes from the above and a hex string, and output the text that the hex string codes for.

This is useful for making long hex strings slightly more user friendly. Here is an example that uses "The Project Gutenberg EBook of Myths and Legends of Ancient Greece and Rome, by E.M. Berens" [http://www.gutenberg.org/files/22381/22381.txt], heretofore "legends.txt".  Any large book-length text (I suggest a selection from Project Gutenberg) will work nicely.

     # creates "legends.4", containing a set of codes for every set of 4 letters in the text:
     hamminghash c legends.txt legends.4 4
     # hash the first 4 bytes of the md5sum of "legends.txt" itself:
     hamminghash legends.4 6e2ee6a5

The output of which is:
    
    the processal
    river,

which has 32 bits of entropy, but is arguably easier to remember than '6e2ee6a5', should you need to.

### Problems:

- The code is a mess. (The file that contains `main' here is LetterCode.hs, inexplicably, for example)

- It chokes on some non-ASCII input text.

- Surely many other minor things.