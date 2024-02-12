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
        """2 AM
7
13
22
1989
...Ready for It?
123 Let's go bitch
123 lgb
2 Soft 4 All of It
2003 Unbearable
2190 days
300 takeout coffees
4Got That U Existed
90s Trend
A little gauche
A Perfectly Good Heart
A Place in This World
Acid Rock
Afterglow
Ahead of the Curve
Alchemy
Alcott
All 4 U Like Janet
All At Once
All By Design
All Eyes On Us
All I Think About Is Karma
All Of The Girls U ♡ B4
All of the Girls You Loved Before
All Too Well
All Too Well\u{00A0}10 min 
All You Had to Do Was Stay
Alls Well That Ends Well
Always Remember
American Girl\u{00A0}
Angels Roll Their Eyes
Another Drama
Anti Hero
AOTGYLB
APGH
APITW
April 29 
Archer
Are We Falling
Arent U Envious
argumentative, antithetical dream girl
Arm Lyrics
At Tea Time
ATW
ATW10MV
ATWTMVTVFTV
August
August Slipped Away
Aurora borealis
Auroras and Sad Prose
auroras and sad prose
Autumn Leaves
AYHTDWS
Babe
Babe 4 The Weekend
Babe\u{00A0}Taylors Version
Back to December
Back up Baby
Bad Blood
Bad Feeling
Bad Surprise
Bait and switch
Bandit Like Me
BBB
BDILH
Beautiful Eyes
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
Big Old City
Big Reputation
Bigger Than the Whole Sky
Bitch not a Baller
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
Burnin Red
But Daddy I Love Him
Bye Bye Baby
Calamitous ♡ 
Call It What You Want
Can I Go Where U Go
Capture It
Cardigan
Careful Daughter
Carolina
Castles Crumbling
Casually Cruel
Catlyn
Cats
Cause Shes Dead
CAY
CBBH
Chai Cookies
Champagne Problems
Cheer Captain
Christmas Tree Farm
CIWTR
CIWYW
Clandestine meetings
Clara Bow
Clean
cleaning up bottles with u
CLM
Close Your Eyes
Closure
CMBSM
Coat of arms
Cold as You
Come Back Be Here
Come in with the Rain
Coney Island
Cornelia St
Cornelia Street
Covered in U
Covert Narcissism
Cowboy like Me
Crazier
Crooked ♡
Crooked love in a straight line down
Cruel Summer
Cryptic and machiavellian
CTF
Cuz Shes Dead
CWYWM
Dancing Witch
Dancing with Our Hands Tied
Dangerous Game
Daylight
DBATC
DBM
Dear John
Dear Reader
Death By 1000 Cuts
Death by a Thousand Cuts
Delicate
Devils Roll The Dice
Diamonds N My Eyes
Did I Shatter You
Don't Blame Me
Don't Call Me Baby
Don't Say Yes
Don't You
Dorothea
Double Vision
Down Bad
Down In Flames
Dream Impossible
Dress 
Dress 2 Kill
Dressed to the 9's
Dressing 4 Revenge
Drop Everything Now
Drunk on Jealousy
DWOHT
EHC
Electric Touch
Elegies eulogize me
Enchanted
End Game
Epiphany
Eras
Eras Tour
Even Better
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
Feel This Magic
Fell down a rabbit hole
Fifteen
Fighting Dragons
Fireworks Show
Florida
Folklore
Follow The Sparks
Foolish One
Forever & Always
Forever and Always
Forever and Ever
Forever Winter
Fortnight
FOTS
Fresh Out the Slammer
From the Vault
Fuck the patriarchy
Fuck U 4Ever
Fucked In The Head
Fuckin Beautiful
GAH
Gardens of Babylon
GAS
Gasoline
Georgia Stars
Get Even
Get It Off My Desk
Getaway Car
Girl at Home
Give U My Wild
Gleaming
Glitch
glitter on the floor
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
Haters Gonna Hate
Haunted
Haylor
He Did It
He Got My ♡ Beat
He was Sunshine
He's a gentleman
Headfirst (Fearless)
Heartbreak Prince
Hella good hair
Hey Stephen
Hi, it's me
High in the Sky
High Infidelity
Hit's Different
Hoax
Holy Ground
Home 2 My Cats
How U Get the Girl
How You Get the Girl
HYGTG
I ♡ TS
I Almost Do
I Bet U Think About Me
I Bet You Think About Me
I Can Do It With a Broken Heart
I Can Fix Him (No Really I Can)
I Can See You
I Did Something Bad
I Don't ♡ The Drama It ♡ Me
I Don't Dress 4 Men
I Don't Dress 4 Women
I don't like acid rock
I Don't Remember
I Don't Start Shit
I Don't Wanna Live 4Eva
I Don't Wanna Live Forever
I Fancy U
I Forgot That You Existed
I Heart ? 
I Hope They Shine
I Knew You Were Trouble
I Know Places
I Miss Sparkling
I Shine So Bright
I survived
I Think He Knows
I was there
I Wish U Would
I Wish You Would
I'll Be Strong
I'm a Fire
I'm All About U
I'm Doin Better
I'm Gonna Be Hungover
I'm in a New Hell
I'm Insane
I'm Not Even Sorry
I'm NYC
I'm Only Me When I'm with You
I'm The Problem
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
In my best dress
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
Isn't It
It was Rare
it's Been Waiting 4 U
it's Joever
it's Nice to Have a Friend
it's So Romantic
it's Time to Go
ITHK
Its Been Waiting 4 U
Its Nice 2 Have A Friend
Its So Romantic
Its Time 2 Go
ITTG
ITWAM
Ivy
IWYW
James Dean Daydream
Je Suis Calme
Joever
Joker and Queen
JTF
Jump Then Fall
Junior Jewels
Just a Girl
Just Say Yes
Just So Funny
Just So Pretty
Karma
Karma Is a Cat
Karma Is My BF
Keep Him 4Ever
Key Lime Green
King of My Heart
Kissin in the Rain
Know You Better
KOMH
Labyrinth
Lakes
Last Kiss
Lavender Haze
Let The Games Begin
Let's go bitch
LGB
Life in Pictures
Light Me Up
Lights Down Low
Lights Go Wild
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
Lovely Jewel
Lover
Loving Him Was Red
LSS
LWYMMD
Lyrical Smile
MAATHP
Macavity
Mad Love
Mad Woman
Made of Starlight
Mama Swift
Manuscript
Marjorie
Maroon
Marry Me Juliet
Marvelous Time
Mary's Song
Mary's Song Oh My My My
Mastermind
Me And Karma Vibe
Me Hee Hee
Me!
Mean
Meet Me At Midnight
Mei
Meredith 
MEREDITH GREY
Message in a Bottle
MIAB
Midas Touch
Midnight Rain
Midnights
Midnights like This
Mine
Mirrorball
Miss Americana
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
My Favorite Song
My First Concert
My Mind Is Alive
My Tears and My Beers
My Tears Ricochet
My Time My Wine
My Whole World
MYOBHFT
Mystical TIme
Nashville
NBNC
Need
Never Gonna Love Again
Never Grow Up
Never Mine
New Romantics
New Years Day
New York, no shoes
Next Chaper
NGU
Nice
Nights R So Starry
No Body No Crime
No it's Becky
No One Understands
Nobodys Listening
Not Fancy Stuff
Not Fine At All
Not Like The Regulars
Nothin in my Brain
Nothing New
November Flush
Now I'm Ur Daisy
Now That We Don't Talk
NY
Ocean Wave Blues
Oh Goddamn
Oh My My My
Old Pickup Truck
Olive Garden
Olivia
OLVIA BENSON
OMG
On a Wishin Star
On Some New Shit
On the Bleachers
Once Upon a Time
Only See Daylight
Only the Young
OOTW
OTY
Our Song
Ours
Out of the Woods
Over and Over
Paper Airplanes
Paper Rings
Paris
Passionate as Sin
Peace
Peppermint Candy
Permanent mark
Peter Losing Wendy
Picture to Burn
Pitch black ink
Players Gonna Play
Poets 
Poets Department
Produced By
Promise Me This
PTB
Purple Pink Skies
Put On Your Records
Question
R Stoned Swifties
R Taylor Swift
R We Out Of The Woods
Ratatatatata 
Ready 4 Combat
Ready 4 It
Real Fuckin Legacy
Red
Red Lip Classic
Remember It
Remember this Feelin
Renegade
Rep
Reputation
RFI
Right Where U Left Me
Right Where You Left Me
Ronan
Rose Blush
roses
Ruining Everything
Rumors Fly
Run
RWYLM
Sabotage
Sad Beautiful Tragic
Sad Girl Autumn
Safe and Sound
Salt Air
Sapphire Tears
Say Don't Go
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
Shut Up Wench
SIG
SIO
SLL
Slut
Snake
Snow on the Beach
So Gatsby
So It Goes…
So Long London
So Scarlet
Soft and Slow
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
Stars by the pocketful
Start of an Age
State of Grace
State the Obvious
Stay Beautiful
Stay Here 4Ever
Stay Stay Stay
Stolen Lullabies
Story of Us
Style
Suburban Legends
Summer ♡
Superman
Superstar
Sweet Kind Fun
Sweet Like Honey
Sweet Nothing
Sweet Tea N The Summer
Sweeter than Fiction
Sweetest Con
Swift
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
Taylors Version
taylurking
TBD
Tea Time
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
These raisins
Thirteen
This Is Me Trying
This is What You Came For
This Is Why We Cant Have Nice Things
This Love
This Sick Beat
Thug Story
Tied Together
Tied Together with a Smile
Tim McGraw
Time of my Life
Timeless
TIMT
Tis the Damn Season
Tis The Damn Szn
TIWWCHNT
TLGAD
TLO
TLT
TMIK
TMW
Tolerate It
TOMG
Tore It All Up
Tortured Poets
Tortured Poets Department
TOSOTD
Treacherous
TS
TS - Debut
TSMWEL
TSOU
TTDS
TTPD
TTWAS
TVFN
TWAF
TWILY
Twin Fire Signs
Twinkling
U ♡ The Game
U Always Have Been
U Are The Best Thing
U Could Be The One
U Fancy Me
U Made Her Like That
U R In Love
U R What U ♡
U Should Find Another
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
we come back every time
We Were Happy
Weird
Welcome 2 New York
Welcome to New York
Whats a Girl Gonna Do
When Emma Falls in Love
White Horse
Who Could Stay
Whos Afraid of Little Old Me
Wildest Dreams
Wildfire Lies
Willow
Wishful Thinking
With You I'd Dance
Wonderland
Wonderstruck
Worship this ♡
Worst of Crimes
Would U Have Me
Would U Want Me
Would've Could've Should've
Wreck My Plans
WTNY
WWH
YAIL
YAOM
YBWM
Yes Whale!
YNS
YNTCD
You All Over Me
You Are in Love
You Belong with Me
You Need to Calm Down
You're Alive
You're Losing Me
You're Not Sorry
You're on Your Own Kid
You're Speechless
You're The West Village
Your Illusionist
YOYOK"""
