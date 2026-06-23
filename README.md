This directory, i.e. `VerseSemantics/`, also has a public repo, `https://github.com/augustss/verse-semantics`.

## Push to private repo
If you make changes in the `VerseSemantics/` directory and do a
```
  git push
```
the change will go to the private repo (i.e. `verse-paper`), just as before.
Nothing has changed.


## Push to public repo
To push the changes to the public repo you need to do the following
```
  cd <the verse-paper directory>
  git subtree push --prefix=VerseSemantics git@github.com:augustss/verse-semantics.git main
```


## Pull from public repo
To pull changes from the public repo you need to do the following
```
  cd <the verse-paper directory>
  git subtree pull --prefix=VerseSemantics git@github.com:augustss/verse-semantics.git main --squash
```
