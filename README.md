# thesis
In an effort to save others from the pain and suffering that I have gone through to get the LaTeX and the BYU Thesis/Dissertations package to do what I wanted, I cleaned it up and made some changes. It tries to be a small, neat, clean tutorial (which I don't know how good of a job it does), but it provides all of the necessary details and a good structure to start from. The byustyle pacakge follows the model of the hyperref package (it has a few optional parameters and then a setup function to configure everything). I've tried to put comments around all of the commands to make it easier to make it do what you want but the general run down is this.

Hopefully, this will help stop people from pulling out as many hairs as I have.
Dave Johansen

## Template Use Instructions:
1. You'll need to set all of the name properties or you can make them use defaults (big ugly text that says it's missing) by using the option usedefaultnames.
1. The copyrightyear will just set itself to the current year, but you can change that if you want.
1. You can turn off any part of the BYU header using the commands in the `\byustylesetup{}` on line 28 through 38.
1. Let's say that you just want to print one chapter for some hard copy editing but you don't want to screw up all of the chapter numbers and bibTeX stuff, then you'll notice some commands starting on line 121. Those commands will make LaTeX only put that chapter in the generated document but leave all of the numbering and such untouched. You can also do multiple chapters by doing stuff like `\includeonly{synchronization,results}`. It's a nice little trick that I've really liked using.

Printing Tips:
1. When you print your PDF make sure that "Page Scaling" is set to "None". The default value is "Fit to Printer Margins" and this will cause everything to be not quite the right size.
1. Also, check to make sure that your document is the right size (8.5" x 11"). It has been discovered that several LaTeX installations default to generating a document that is on A4 paper and this will cause your margins to be off throughout your document when you print. To fix this follow these steps (with MikTeX):
  1. Go to the `dvipdfm\config` folder in your MikTeX installation ("C:\texmf" by default) and edit the "config" file in a text editor
	2b) Change "p a4" to "p letter"

	1. Go to the `dvips\config` folder in your MikTeX installation ("C:\texmf" by default) and edit the "config.ps" file in a text editor
	1. Make sure that "letterSize" is the first entry in the list of paper sizes by changing the order from
```
@ A4size 594.99bp 841.99bp
@+ ! %%DocumentPaperSizes: a4
@+ %%PaperSize: A4

@ letterSize 8.5in 11in
@+ ! %%DocumentPaperSizes: Letter
```

to

```
@ letterSize 8.5in 11in
@+ ! %%DocumentPaperSizes: Letter

@ A4size 594.99bp 841.99bp
@+ ! %%DocumentPaperSizes: a4
@+ %%PaperSize: A4
```
