beamer-stub
==============

Scratch template For Rweave + Beamer presentations.

To create a new presentation from this template (modified from https://help.github.com/articles/duplicating-a-repository on 15 MAY 2013):

1. create new github repo: new-repo

2. go to some appropriate filesystem location on command line

3. then:

    ```
    $ git clone --bare https://github.com/pearsonca/beamer-stub.git # Make a bare clone of this repo
    $ cd beamer-stub.git
    $ git push --mirror https://github.com/YOURUSERNAME/new-repo.git # Mirror-push to the new repo
    $ cd ..
    $ rm -rf beamer-stub.git # Remove temporary local repo
    ```
4. This will not immediately create a local repo.  If using github gui, you should now be able to refresh your github repos, and see the new one.  Clone it to local as normal.

5. If using Git + TeXlipse Eclipse plugins, you can now "Import -> Git -> Projects from Repository -> Local", add the new local repo, and then select it as the project source.

6. If you already have the beamer-stub locally, you can skip to the "git push..." step in 3 and then skip the remaining commands in 3.

===============

LICENSE

http://creativecommons.org/licenses/by-nc-sa/3.0/
