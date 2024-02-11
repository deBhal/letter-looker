module SongNames exposing (..)

import Bag
import Regex


spacesRegex : Regex.Regex
spacesRegex =
    Maybe.withDefault Regex.never <| Regex.fromString "\\s"


nonalphanumeric : Regex.Regex
nonalphanumeric =
    Maybe.withDefault Regex.never <| Regex.fromString "[^a-zA-Z0-9]"


sanitize : String -> String
sanitize s =
    String.toLower <| Regex.replace nonalphanumeric (\_ -> "") s


stringToBag : String -> Bag.Bag Char
stringToBag s =
    Bag.fromList (String.toList <| sanitize s)


bagToString : Bag.Bag Char -> String
bagToString b =
    String.fromList (Bag.toList b)


{-| See if the second string contains the letters in the first one ignoring ordering.

    containsLetters "the" "theory" == True

    containsLetters "teh" "theory" == True

    containsLetters "hat" "theory" == False

    containsLetters "THE" "theory" == False

-}
containsLetters : String -> String -> Bool
containsLetters smaller larger =
    let
        smaller_ =
            stringToBag smaller
    in
    Bag.size smaller_
        == Bag.size (Bag.intersect (stringToBag larger) smaller_)


missingLetters : String -> String -> String
missingLetters phrase letters =
    bagToString <| Bag.diff (stringToBag phrase) (stringToBag letters)


regexFlags : Regex.Options
regexFlags =
    { caseInsensitive = True, multiline = True }


removeAtMostLetters : Char -> Int -> String -> String
removeAtMostLetters c n s =
    let
        regex =
            Maybe.withDefault Regex.never <| Regex.fromStringWith regexFlags (String.fromChar c)
    in
    Regex.replaceAtMost n regex (\_ -> "") s



{- Remove the letters in the second string from the first.
   -- The order of the first string is preserved with the earliest instances
   -- of letters being removed.
   -- The order of letters to be removed doesn't matter.
-}


removeLetters : String -> String -> String
removeLetters s b =
    Bag.foldl removeAtMostLetters s <| Bag.fromList <| String.toList b


songs : List String
songs =
    String.split "\n"
        """123 lets go bitch
123 lgb
2 Soft 4 All of It
2003 Unbearable
90s Trend
A Perfectly Good Heart
A Place in This World
Afterglow
Ahead of the Curve
Alchemy
Alcott
All 4 U Like Janet
All At Once
All By Design
All Eyes On Us
All I Think About Is Karma
All of the Girls You Loved Before
All Too Well
All Too Well\u{00A0}10 min 
All You Had to Do Was Stay
Always Remember
American Girl\u{00A0}
Angels Roll Their Eyes
Another Drama
Anti Hero
APGH
APITW
Archer
Are We Falling
Arent U Envious
Arm Lyrics
At Tea Time
ATW
ATW10MV
August
August Slipped Away
Auroras and Sad Prose
AYHTDWS
Babe
Babe 4 The Weekend
Babe\u{00A0}Taylors Version
Back to December
Bad Blood
Bad Feeling
Bad Surprise
Bandit Like Me
BBB
BDILH
Beautiful Fool
Beautiful Ghosts
Begin Again
Bejeweled
Benjamin 
BENJAMIN BUTTON 
Benji
Best Day
Better Man
Better than Revenge
Betty
Big Reputation
Bigger Than the Whole Sky
Bitch Not a Baller
bitch not baller
Bitch Pack Friends
Blank Space
Bless My Soul
Blood Moonlit
Bluest Skies
Bonnie And Clyde
Breathe
Breathe Deep
Breathe In
Bright Lights
BTD
BTR
BTTWS
Burnin It Down
But Daddy I Love Him
Bye Bye Baby
Call It What You Want
Can I Go Where U Go
Cardigan
Castles Crumbling
Casually Cruel
Catlyn
Cats
Cause Shes Dead
CAY
CBBH
Champagne Problems
Christmas Tree Farm
CIWTR
Clara Bow
Clean
CLM
Closure
CMBSM
Coat of arms
Cold as You
Come Back Be Here
Come in with the Rain
Coney Island
Cornelia Street
Covered in U
Cowboy like Me
Cruel Summer
CTF
Cuz Shes Dead
CWYWM
Dancing with Our Hands Tied
Dangerous Game
Daylight
DBATC
DBM
Dear John
Dear Reader
Death by a Thousand Cuts
Delicate
Devils Roll The Dice
Diamonds N My Eyes
Did I Shatter You
Dont Blame Me
Dont Call Me Baby
Dont You
Dorothea
Double Vision
Down Bad
Down In Flames
Dress
Dress 2 Kill
Dressing 4 Revenge
Drunk on Jealousy
DWOHT
EHC
Electric Touch
Enchanted
End Game
Epiphany
Eras
Eras Tour
Evermore
Everybody Agrees
Everything Has Changed
Exile
Eyes Full of Stars
Eyes Open
FAA
False God
Fancy Shit
Fearless
Fifteen
Fighting Dragons
Fireworks Show
Florida
Folklore
Follow The Sparks
Foolish One
Forever & Always
Forever and Ever
Forever Winter
Fortnight
FOTS
Fresh Out the Slammer
From the Vault
fuck jake
fuck kanye
Fuck the patriarchy
Fuck U 4Ever
Fucked In The Head
Fuckin Beautiful
GAH
Gardens of Babylon
GAS
Get Even
Get It Off My Desk
Getaway Car
Girl at Home
Give U My Wild
Gleaming
Glitch
Go Viral
Going Out Tonight
Gold Rush
Golden Like Daylight
Good Girl Faith
Gorgeous
Grammys
Great War
Guilty as Sin
Half Moon Eyes
Handsome as Hell
Happiness
Happy Birthday
Haters Gonna Hate
Haunted
Haylor
He Did It
He was Sunshine
Hey Stephen
High in the Sky
High Infidelity
Hits Different
Hoax
Holy Ground
Home 2 My Cats
How You Get the Girl
HYGTG
I Almost Do
I Bet You Think About Me
I Can Do It With a Broken Heart
I Can Fix Him No Really I Can
I Can See You
I Did Something Bad
I Dont Dress 4 Men
I Dont Dress 4 Women
I Dont Remember
I Dont Start Shit
I Dont The Drama It Me
I Dont Wanna Live Forever
I Fancy U
I Forgot That You Existed
I Knew You Were Trouble
I Know Places
I Miss Sparkling
I Shine So Bright
I survived
I Think He Knows
I TS
I Wish You Would
IAD
IBYTAM
ICDIWABH
ICFINRIC
Idiotic Fool
IDK
IDSB
IDWLF
If This Was a Movie
IFTYE
IKP
IKYWT
Illicit Affairs
Im a Fire
Im All About U
Im Doin Better
Im Insane
Im Not Even Sorry
Im NYC
Im Only Me When Im with You
Im The Problem
In My Dreamland
In My Dreams
In My Feelings
In My Hometown
in my monologe
In the Silence ♡
Innocent
INTHAF
Invisible
Invisible String
IOMWIWY
Is It Over Now
Island Breeze
Isnt It
ITHK
Its Been Waiting 4 U
its Joever
Its Me Hi
Its Nice to Have a Friend
Its So Romantic
Its Time to Go
ITTG
ITWAM
Ivy
IWYW
James Dean Daydream
Je Suis Calme
joever
Joker and Queen
JTF
Jump Then Fall
junior jewels
Just So Pretty
Karma
Karma Is a Cat
Karma Is My BF
Keep Him 4Ever
Key Lime Green
King of My Heart
KOMH
Labyrinth
Lakes
Last Kiss
Lavender Haze
Let The Games Begin
Light Me Up
Lights Down Low
Lillith
Live With Ghosts
loml
London
London Boy
Long Live
Long Story Short
Long Time Comin
Look What You Made Me Do
Looks Can Kill
Love Blackout
Love bombs
Love Spiral
Love Story
Loved U N Secret
Lover
Loving Him Was Red
LSS
LWYMMD
Lyrical Smile
MAATHP
Macavity
Mad Love
Mad Woman
Made Me Crazy
Mama Swift
Manuscript
Marjorie
Maroon
Marry Me Juliet
Marvelous Time
Marys Song
Marys Song Oh My My My
Mastermind
Me
Me And Karma Vibe
Me Hee Hee
Mean
Meet Me At Midnight
Meredith 
MEREDITH GREY
Message in a Bottle
MIAB
Midnight Rain
Midnights
Midnights like This
Mine
Mirrorball
Miss Americana & the Heartbreak Prince
Miss Me N Ur Bones
Monster on the Hill
MPF
Mr Perfectly Fine
Ms Misery
MTR
muses 
My Auras Moonstone
My Boy Only Breaks His Favorite Toys
My Centerfold
My Drug Is My Baby
My Tears and My Beers
My Tears Ricochet
My Time My Wine
My Whole World
MYOBHFT
Mystical Time
Nashville
NBNC
Need
Never Gonna Love Again
Never Grow Up
Never Mine
New Romantics
New Years Day
NGU
Nice
Nights R So Starry
No Body No Crime
No Its Becky
No One Understands
Not Fancy Stuff
Not Like The Regulars
Nothin in my Brain
Nothing New
November Flush
Now Im Ur Daisy
Now That We Dont Talk
NY
Ocean Wave Blues
Oh Goddamn
Olive Garden
Olivia
OLVIA BENSON
OMG
On Some New Shit
Only See Daylight
Only the Young
OOTW
OTY
Our Song
Ours
Out of the Woods
Over and Over
Paper airplanes
Paper Rings
Paris
Peace
Peppermint Candy
Peter Losing Wendy
Picture to Burn
pitch black ink
Players Gonna Play
Poets 
Poets Department
PTB
Purple Pink Skies
Put On Your Records
Question
R We Out Of The Woods
Ratatatatata 
Ready 4 Combat
Ready for It
Real Fuckin Legacy
Red
Red Lip Classic
Renegade
Rep
Reputation
RFI
Right Where You Left Me
Ronan
Rose Blush
roses
Ruining Everything
Rumors Fly
Run
RWYLM
Sad Beautiful Tragic
Safe and Sound
Salt Air
Sapphire Tears
Say Dont Go
SBT
Scared of Ghosts
Scorpion Sting
SCREAMING COLOUR 
September\u{00A0}
Sequin Smile
Seven
Sexy Baby
Shades of Wrong
Shake It Off
Shimmer
Shiny Friends
Shiny Things
Shouldve Said No
SIG
SIO
SLL
Slut
swift
snake
Snow on the Beach
So Gatsby
So It Goes
So Long London
So Scarlet
SoG
Soon Youll Get Better
SOTB
Sparks Fly
Speak Now
Spelling is Fun
Spin
Squad
SSN
SSS
Starbucks Lovers
Starlight
Starry Eyes
State of Grace
Stay Beautiful
Stay Here 4Ever
Stay Stay Stay
Stolen Lullabies
Style
Suburban Legends
Summer 
Superman
Superstar
Sweet Kind Fun
Sweet Like Honey
Sweet Nothing
Sweet Tea N The Summer
Sweeter than Fiction
Sweetest Con
Swiftie
Sydney
SYGB
T Swizzle
Take Me Home
Take the Moment
Taken By the View
talismans
Tallest Tiptoes
Tay Tay
Taylor
Taylor Swift
Taylors Vers
taylurking
TBD
tea time
team jacob
Teardrops on My Guitar
Tell Me Why
Thats When
The 1
The Alchemy
The Alcott
The Archer
The Best Day
The Great War
The Hope of it All
The Joker and the Queen
The Lakes
The Last Great American Dynasty
The Last Time
The Lucky One
The Man
The Manuscript
The Moment I Knew
The Moon Is High
The Other Side of the Door
The Outside
The Smallest Man Who Ever Lived
The Story of Us
The Tortured Poets Department
The Very First Night
The Way I Loved You
Thirteen
This Is Me Trying
This is What You Came For
This Is Why We Cant Have Nice Things
This Love
This Sick Beat
Tied Together with a Smile
Tim McGraw
Timeless
TIMT
Tis the Damn Season
TIWWCHNT
TLGAD
TLO
TLT
TMIK
TMW
Tolerate It
TOMG
Tortured Poets
Tortured Poets Department
TOSOTD
Treacherous
TS
TSMWEL
TSOU
TTDS
TTPD
TTWAS
TVFN
TWAF
TWILY
Twinkling
U 2 the Moon and 2 Saturn
U Always Have Been
U Fancy Me
U Made Her Like That
U R What U 
U Should Find Another
U The Game
Untouchable
Ur Being 2 Loud
ur gay
Vault
Vigilante Shit
WANEGBT
Want Ur Midnights
WAOLOM
WCS
We Are Never Ever Getting Back Together
We Were Happy
Weird
Welcome to New York
Whats a Girl Gonna Do
When Emma Falls in Love
White Horse
Who Could Stay
Whos Afraid of Little Old Me
Wildest Dreams
Willow
Wonderland
Worship this 
Worst of Crimes
Would U Have Me
Would U Want Me
Wouldve Couldve Shouldve
Wreck My Plans
WTNY
WWH
YAIL
YAOM
YBWM
yes whale
YNS
YNTCD
You All Over Me
You Are in Love
You Belong with Me
You In Secret
You Need to Calm Down
Your Illusionist
Youre Alive
Youre Losing Me
Youre Not Sorry
Youre on Your Own Kid
Youre Speechless
Youre The West Village
YOYOK
mei
mei mei"""
