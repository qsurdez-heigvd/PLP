module HTML where

import Prelude hiding (div, head)
import Data.List (intercalate, head)

-- How to represent my elements ? Type or functions ? 
-- A Html page is made of elements. These elements can be tags, these elements have attributes
-- I think going for type safety is quite a safe bet, exposing functions to use these types can be interesting 
-- to match the exemple given
-- I went in the direction of type safety but I guess this doesn't make a lot of sense in the end as we want to expose 
-- the API to the client and we don't really care about type safety in this lab. I wanted to do too much then ? 
-- No just writing functions that build the elements is great so we can have both an API and Type Safety ! Yay


-- Html doc is composed of a HeadSection and a BodySection
data Html = HtmlDoc HeadSection BodySection
  deriving (Show)

-- Only specific elements can be in the head https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements#document_metadata
data HeadSection = HeadSection [HeadElement]
  deriving (Show)

-- Only specific elements can be in the body https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements#content_sectioning
data BodySection = BodySection [BodyElement]
  deriving (Show)

data HeadElement
  = Title String
  | Link String String -- rel, href
  deriving (Show)

-- Type Safety
data BodyElement
  = H1 String
  | H2 String
  | H3 String
  | H4 String
  | H5 String
  | H6 String
  | P String
  | Div (Maybe String) [BodyElement] -- for me this attribute could be omitted and this allows me to practice Maybe which I desperatly need
  | Ul [ListItem]
  | A String String -- href, content
  | Img String String -- src, alt
  | Br
  | Hr
  deriving (Show)

data ListItem = Li String
  deriving (Show)

-- I choose arbitrarily which tags have an indentation and which do not
render :: Html -> String
render html = renderWithIndent 0 html

renderWithIndent :: Int -> Html -> String
renderWithIndent level (HtmlDoc headEl bodyEl) = 
  indent level ++ "<html>\n" ++ 
  renderHeadWithIndent (level + 1) headEl ++
  renderBodyWithIndent (level + 1) bodyEl ++
  indent level ++ "<html/>"

-- I think better than line breaks is a function that would indent with the correct 
-- level
indent :: Int -> String
indent level = replicate (level * 2) ' '

renderHead :: HeadSection -> String
renderHead (HeadSection elements) =
  "<head>\n\t" ++ concatMap renderHeadElement elements ++ "<head/>\n"

renderHeadWithIndent :: Int -> HeadSection -> String
renderHeadWithIndent level (HeadSection elements) = 
  indent level ++ "<head>\n" ++
  concatMap (renderHeadElementWithIndent (level + 1)) elements ++ 
  indent level ++ "<head/>\n"

renderHeadElement :: HeadElement -> String
renderHeadElement (Title str) = "<title>" ++ str ++ "<title/>\n"
renderHeadElement (Link rel href) = "<link rel=\"" ++ rel ++ "\"" ++ "href=\"" ++ href ++ "\"/>\n"

renderHeadElementWithIndent :: Int -> HeadElement -> String
renderHeadElementWithIndent level (Title str) =
  indent level ++ "<title>" ++ str ++ "<title>\n"
renderHeadElementWithIndent level (Link rel href) =
  indent level ++ "<link rel=\"" ++ rel ++ "\"" ++ "href=\"" ++ href ++ "\"/>\n"

renderBody :: BodySection -> String
renderBody (BodySection elements) =
  "<body>" ++ concatMap renderBodyElement elements ++ "<body/>\n"

renderBodyWithIndent :: Int -> BodySection -> String
renderBodyWithIndent level (BodySection elements) = 
  indent level ++ "<body>\n" ++
  concatMap (renderBodyElementWithIndent (level + 1)) elements ++
  indent level ++ "<body/>\n"

-- Old version without indentation ^^
renderBodyElement :: BodyElement -> String
renderBodyElement (H1 str) = "<h1>" ++ str ++ "<h1/>\n"
renderBodyElement (H2 str) = "<h2>" ++ str ++ "<h2/>\n"
renderBodyElement (H3 str) = "<h3>" ++ str ++ "<h3/>\n"
renderBodyElement (H4 str) = "<h4>" ++ str ++ "<h4/>\n"
renderBodyElement (H5 str) = "<h5>" ++ str ++ "<h5/>\n"
renderBodyElement (H6 str) = "<h6>" ++ str ++ "<h6/>\n"
renderBodyElement (P str) = "<p>" ++ str ++ "<p/>\n"
renderBodyElement (Div Nothing children) =
  "<div>\n\t" ++ concatMap renderBodyElement children ++ "<div/>\n"
renderBodyElement (Div (Just classStr) children) =
  "<div class=\"" ++ classStr ++ "\">\n\t" ++ concatMap renderBodyElement children ++ "<div/>\n"
renderBodyElement (Ul items) =
  "<ul>\n\t" ++ concatMap renderListItem items ++ "<ul/>\n"
renderBodyElement (A href content) =
  "<a href=\"" ++ href ++ "\">" ++ content ++ "<a/>\n"
renderBodyElement (Img src alt) =
  "<img src=\"" ++ src ++ "\" alt=\"" ++ alt ++ "\" />\n"
renderBodyElement Br = "<br/>\n"
renderBodyElement Hr = "<hr/>\n"

renderBodyElementWithIndent :: Int -> BodyElement -> String
renderBodyElementWithIndent level (H1 str) = 
  indent level ++ "<h1>" ++ str ++ "<h1/>\n"
renderBodyElementWithIndent level (H2 str) = 
  indent level ++ "<h2>" ++ str ++ "<h2/>\n"
renderBodyElementWithIndent level (H3 str) = 
  indent level ++ "<h3>" ++ str ++ "<h3/>\n"
renderBodyElementWithIndent level (H4 str) = 
  indent level ++ "<h4>" ++ str ++ "<h4/>\n"
renderBodyElementWithIndent level (H5 str) = 
  indent level ++ "<h5>" ++ str ++ "<h5/>\n"
renderBodyElementWithIndent level (H6 str) =
  indent level ++ "<h6>" ++ str ++ "<h6/>\n"
renderBodyElementWithIndent level (P str) = 
  indent level ++ "<p>" ++ str ++ "<p/>\n"
renderBodyElementWithIndent level (Div Nothing children) =
  indent level ++ "<div>\n" ++ 
  concatMap (renderBodyElementWithIndent (level + 1)) children ++ 
  "<div/>\n"
renderBodyElementWithIndent level (Div (Just classStr) children) =
  indent level ++ "<div class=\"" ++ classStr ++ "\">\n" ++ 
  concatMap (renderBodyElementWithIndent (level + 1)) children ++ 
  "<div/>\n"
renderBodyElementWithIndent level (Ul items) =
  indent level ++ "<ul>\n" ++ concatMap (renderListItemWithIndent (level + 1)) items ++ 
  "<ul/>\n"
renderBodyElementWithIndent level (A href content) =
  indent level ++ "<a href=\"" ++ href ++ "\">" ++ content ++ "<a/>\n"
renderBodyElementWithIndent level (Img src alt) =
  indent level ++ "<img src=\"" ++ src ++ "\" alt=\"" ++ alt ++ "\" />\n"
renderBodyElementWithIndent level Br = 
  indent level ++ "<br/>\n"
renderBodyElementWithIndent level Hr = 
  indent level ++ "<hr/>\n"

renderListItem :: ListItem -> String
renderListItem (Li str) = 
  "<li>" ++ str ++ "<li/>\n"

renderListItemWithIndent :: Int -> ListItem -> String
renderListItemWithIndent level (Li str) = 
  indent level ++ "<li>" ++ str ++ "<li/>\n"

-- Gonna make it look like more like the example as this is not necessarily what we wanted ...

html :: [HeadElement] -> [BodyElement] -> Html
html headElements bodyElements = HtmlDoc (HeadSection headElements) (BodySection bodyElements)

-- I choose to be explicit with the arguments, I'm not comfortable enough to remove them
title :: String -> HeadElement
title str = Title str

link :: String -> String -> HeadElement
link rel href = Link rel href

h :: Int -> String -> BodyElement
h 1 str = H1 str
h 2 str = H2 str
h 3 str = H3 str
h 4 str = H4 str
h 5 str = H5 str
h 6 str = H6 str
h _ str = error "Heading level must be between 1 and 6"

p :: String -> BodyElement
p str = P str

div :: String -> [BodyElement] -> BodyElement
div "" children = Div Nothing children
div classStr children = Div (Just classStr) children

ul :: [ListItem] -> BodyElement
ul items = Ul items

li :: String -> ListItem
li str = Li str

a :: String -> String -> BodyElement
a href content = A href content

img :: String -> String -> BodyElement
img src alt = Img src alt

br :: BodyElement
br = Br

hr :: BodyElement
hr = Hr

page :: Html
page = 
  html 
    [title "HTML Page Example",
     link "stylesheet" "/shared-assets/misc/link-element-example.css"]
    [
      h 1 "Welcome to HTML",
      p "This is a paragraph",
      a "https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/a" "This is a link to a documentation",
      br,
      hr,
      div "content" [
        h 2 "A section with a list !",
        ul [
          li "First item",
          li "Second item",
          li "Third item"
        ]
      ],
      img "image.jpg" "An exemple image"
    ]

-- It's a shame, within my ghci console, the \n do not show as newlines. It's thus quite difficule to see wether or 
-- not the indentation is correct :(
