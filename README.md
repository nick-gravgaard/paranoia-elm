paranoia-elm
============

This is an Elm version of the `paranoia` game from the `bsd-games`
package found on many old Unix/Linux systems. Since versions of this
game now exist in various different languages
([C](https://github.com/nickgravgaard/c-paranoia),
[Python](https://github.com/nickgravgaard/py-paranoia),
[Go](https://github.com/nickgravgaard/go-paranoia)), I thought it
could provide an excellent point of reference for comparing different
languages, and show off the advantages of ML style languages like Elm.

Fans of functional languages often talk about the advantages of making
illegal state unprepresentable, and I try to do that here. For
example, look at the potential for error in the "logic" style pages in
the Python and Go code.

History
-------

This game originally took the form of a
[piece of interactive fiction](https://archive.org/details/space-gamer-77/page/n15)
by Sam Shirley set in the Paranoia universe which was published in
issue 77 (Jan/Feb 1987) of SpaceGamer/FantasyGamer magazine.

That was the basis for Tim Lister's C version of the game, which was
subsequently ported to Python and Go by Sean P. Kane.

I wrote the code for this Elm version but copied a lot of the text
from those versions as well as occasionally referring back to
archive.org's scans of the original printed version.

