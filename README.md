Glossary
========

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

This is an R package to add a glossary to an Rmarkdown-based website and
facilitate linking terms to it. Glossary terms are defined in an Rmd
file not rendered on the website. This file is rendered by the package
into HTML and the `<div>`s containing glossary terms are extracted and
modified to contain targets for HTML anchors. An example of a definition
file is included in the pacakge and can be viewed
[here](inst/extdata/example_definitions.Rmd).

    library(glossary)
    glossary_def_path <- system.file(package = "glossary", "extdata/example_definitions.Rmd")
    gloss <- glossary(definitions_path = glossary_def_path) # returns an R6 class

If the glossary is to be on the same page as the terms, then only the
path to the term definition Rmd is needed, otherwise the `glossary_path`
option must be used to specify the path to the Rmd file that will render
the glossary. In this example, we will render the glossary terms on this
page, so the `glossary_path` option was not used.

To link a term, use the `add` function of the glossary object in an
inline R chunk. For example, writing:

------------------------------------------------------------------------

We used the amazing `` `r gloss$add("glossary")` ``
`` `r gloss$add("r package")` `` to make our glossary! Too bad its not
on
`` `r gloss$add("The Comprehensive R Archive Network (CRAN)", shown = "CRAN")` ``
yet.

------------------------------------------------------------------------

Would render to:

------------------------------------------------------------------------

We used the amazing <a href ="#glossary_anchor">glossary</a>
<a href ="#r_package_anchor">r package</a> to make our glossary! Too bad
its not on
<a href ="#the_comprehensive_r_archive_network_(cran)_anchor">CRAN</a>
yet.

------------------------------------------------------------------------

The terms are case-insensitive and the `shown` option can be used to
change what text is used to link to the term.

The `gloss$render_all()` function can then be used to render the
glossary and the links from the terms used in the text to the rendered
glossary definitions:

Example glossary
----------------

    gloss$render_all()

<h3>
<a class="glossary_anchor" id="class_anchor">Class</a>
</h3>
<p>
A class is a defined set of variables along with a set of functions
designed to work with those variable. The specifics of how classes are
structured vary greatly between programming languages, but the concepts
are similar. For example, you might have a class called “Dog” that
contained the dogs age (number), the dogs breed (text), and the name of
the dogs owner (text). With those variables, the “Dog” class might have
functions that make the dog a year older or change the owner of the dog,
etc.
</p>

<h3>
<a class="glossary_anchor" id="function_anchor">Function</a>
</h3>
<p>
Any command or operation the does something in a programming language is
a function. Functions often have inputs that influence what the output
is, but some don’t have inputs. Functions will usually return some type
of output, but they might not, or they might have an effect besides what
they return (this is rare in R, but common in other programming
languages). The concept of functions, like variables, comes from math.
For example, the equation for a line is <code>y = mx + b</code>. In R,
you could make a function to return <code>y</code> given the values of
<code>m</code>, <code>x</code>, and <code>b</code>, like so:
</p>
<pre class="r"><code>line &lt;- function(m, x, b) {
  return(m * x + b)
}</code></pre>
<p>
And find the value for y, for a given set of inputs like so:
</p>
<pre class="r"><code>line(m = 2.5, x = 3, b = -1)
#&gt; [1] 6.5</code></pre>

<h3>
<a class="glossary_anchor" id="glossary_anchor">Glossary</a>
</h3>
<p>
An R package to add a glossary to an Rmarkdown-based website and
facilitate linking terms to it.
</p>

<h3>
<a class="glossary_anchor" id="object_anchor">Object</a>
</h3>
<p>
An instance of a class. In other words, a data with a defined type and
functions designed to operate on it. For example, if you had a class for
“Dog”, you might have an object of that class stored in a variable
called “fido” and another called “scraps”.
</p>

<h3>
<a class="glossary_anchor" id="r_package_anchor">R package</a>
</h3>
<p>
An R package is a set of user-defined functions organized so that people
can easily share and use them. Most of the functions used by most R
users are from R packages rather than those supplied by base R. R
packages can be installed in a few ways, but the most common is to
download them from The Comprehensive R Archive Network (CRAN) using the
<code>install.packages</code> function. For example <code>stringr</code>
is an R package that supplies functions to work with text.
</p>
<pre class="r"><code>install.packages("stringr")</code></pre>
<p>
Once installed, a package must be “loaded” using the
<code>library</code> function before any functions it supplies can be
used:
</p>
<pre class="r"><code>library("stringr")</code></pre>

<h3>
<a class="glossary_anchor" id="the_comprehensive_r_archive_network_(cran)_anchor">The
Comprehensive R Archive Network (CRAN)</a>
</h3>
<p>
A volunteer-run organization that hosts R packages and enforces
standards for how they should be structured. When you install an R
package using <code>install.packages</code>, you are installing from
CRAN. CRAN is one of the major reasons R packages are so easy to
install.
</p>

<h3>
<a class="glossary_anchor" id="variable_anchor">Variable</a>
</h3>
<p>
In programming, a variable is a name associated with a value that can
change or “vary”. This is similar to how the word is used in math. For
example, the equation for a line is <code>y = mx + b</code>. In this
equation, all of the letters are variables that can represent any
number.
</p>
