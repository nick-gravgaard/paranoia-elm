module Paranoia exposing (..)

import Browser
import Element exposing (..)
import Element.Font as Font
import Element.Input exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Random
import String.Interpolate exposing (interpolate)


type Msg
    = Instructions
    | Page1
    | Page2 Counters
    | Page3
    | Page4
    | Page5
    | Page6
    | Page7
    | Page8
    | Page9
    | Page10
    | Page10Tubecar
    | Page10TubecarResult Bool
    | Page11
    | Page11More
    | Page12
    | Page13
    | Page14
    | Page15
    | Page16
    | Page17
    | Page17Fight Int Int
    | Page17FightResult Int Int (Maybe Int) (Maybe Int)
    | Page18
    | Page18Balance
    | Page18BalanceResult Bool
    | Page19
    | Page19KilledTooMany
    | Page20
    | Page21
    | Page22
    | Page22RandomEncounterResult Msg
    | Page23
    | Page24
    | Page25
    | Page26
    | Page28
    | Page29
    | Page30
    | Page31
    | Page32
    | Page33
    | Page34
    | Page34More
    | Page35
    | Page36
    | Page37
    | Page38
    | Page39
    | Page40
    | Page40Fight
    | Page40FightResult Bool Bool
    | Page41
    | Page42
    | Page43
    | Page44
    | Page45
    | Page46
    | Page47
    | Page48
    | Page49
    | Page50
    | Page51
    | Page52
    | Page53
    | Page54
    | Page55
    | Page55More
    | Page56
    | Page57
    | YouLose
    | ToggleCharSheet


type alias Counters =
    { moxie : Int
    , agility : Int
    , hitPoints : Int
    , clone : Int
    , killerCount : Int
    , maxKill : Int
    , platoClone : Int
    , computerRequest : Bool
    , ultraViolet : Bool
    , actionDoll : Bool
    , readLetter : Bool
    , blastDoor : Bool
    }


type alias Model =
    { text : String
    , choices : List ( String, Msg )
    , counters : Counters
    , showCharSheet : Bool
    }


charSheet : Counters -> String
charSheet counters =
    interpolate """
===============================================================================
The Character : Philo-R-DMD {0}
Primary Attributes                      Secondary Attributes
===============================================================================
Strength ..................... 13       Carrying Capacity ................. 30
Endurance .................... 13       Damage Bonus ....................... 0
Agility ...................... 15       Macho Bonus ....................... -1
Manual Dexterity ............. 15       Melee Bonus ...................... +5%
Moxie ........................ 13       Aimed Weapon Bonus .............. +10%
Chutzpah ...................... 8       Comprehension Bonus .............. +4%
Mechanical Aptitude .......... 14       Believability Bonus .............. +5%
Power Index .................. 10       Repair Bonus ..................... +5%
===============================================================================
Credits: 160        Secret Society: Illuminati        Secret Society Rank: 1
Service Group: Power Services               Mutant Power: Precognition
Weapon: laser pistol to hit, 40% type, L Range, 50m Reload, 6r Malfnt, 00
Skills: Basics 1(20%), Aimed Weapon Combat 2(35%), Laser 3(40%),
        Personal Development 1(20%), Communications 2(29%), Intimidation 3(34%)
Equipment: Red Reflec Armour, Laser Pistol, Laser Barrel (red),
           Notebook & Stylus, Knife, Com Unit 1, Jump suit,
           Secret Illuminati Eye-In-The-Pyramid(tm) Decoder ring,
           Utility Belt & Pouches
===============================================================================
""" [ String.fromInt counters.clone ]


deadCloneChoicesAndCounters : Counters -> (Counters -> Msg) -> ( List ( String, Msg ), Counters )
deadCloneChoicesAndCounters counters countersToMsg =
    let
        newCloneMsg =
            countersToMsg counters

        text =
            interpolate "\u{1FAA6} Clone {0} just died." [ String.fromInt counters.clone ]
    in
    if counters.clone >= 6 then
        ( [ ( text, YouLose ) ], initCounters )

    else
        ( [ ( text, newCloneMsg ) ]
        , { counters
            | clone = counters.clone + 1
            , ultraViolet = False
            , actionDoll = False
            , hitPoints = 10
            , killerCount = 0
          }
        )


next : Msg -> List ( String, Msg )
next msg =
    [ ( "➡️ Next", msg ) ]


more : Msg -> List ( String, Msg )
more msg =
    [ ( "⬇️ More...", msg ) ]


restart : List ( String, Msg )
restart =
    [ ( "🔁 Restart", Instructions ) ]


rollDice : Int -> Int -> Random.Generator Int
rollDice num sides =
    Random.list num (Random.int 1 sides) |> Random.andThen (\a -> List.sum a |> Random.constant)


skillTestGen : Int -> Int -> Int -> Random.Generator Bool
skillTestGen num sides skill =
    rollDice num sides
        |> Random.andThen (\hitRoll -> hitRoll <= skill |> Random.constant)


successText : Bool -> String
successText success =
    if success then
        "YES - you did it!"

    else
        "Oh no, you didn't make it!"


instructionsModel : Model
instructionsModel =
    { text = """
Welcome to Paranoia!

HOW TO PLAY:
   Just press <Enter> until you are asked to make a choice.
   Select 'a' or 'b' or whatever for your choice, then press <Enter>.
   You may select 'p' at any time to get a display of your statistics.
   Always choose the least dangerous option.  Continue doing this until
   you win. At times you will use a skill or engage in combat and will
   be informed of the outcome.  These sections will be self explanatory.

HOW TO DIE:
   As Philo-R-DMD you will die at times during the adventure.
   When this happens you will be given a new clone at a particular
   location. The new Philo-R will usually have to retrace some of
   the old Philo-R's path, but hopefully he won't make the same mistake
   as his predecessor.

HOW TO WIN:
   Simply complete the mission before you expend all six clones.
   If you make it, congratulations.
   If not, you can try again later.
"""
    , choices = next Page1
    , counters = initCounters
    , showCharSheet = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        counters =
            model.counters

        textAndChoices : String -> List ( String, Msg ) -> ( Model, Cmd Msg )
        textAndChoices text choices =
            textAndChoicesAndCounters text choices model.counters

        textAndChoicesAndCounters : String -> List ( String, Msg ) -> Counters -> ( Model, Cmd Msg )
        textAndChoicesAndCounters text choices c =
            ( { text = text, choices = choices, counters = c, showCharSheet = False }, Cmd.none )
    in
    case msg of
        Instructions ->
            ( instructionsModel, Cmd.none )

        Page1 ->
            textAndChoices
                """
You wake up face down on the red and pink checked E-Z-Kleen linoleum floor.
You recognise the pattern, it's the type preferred in the internal security
briefing cells.  When you finally look around you, you see that you are alone
in a large mission briefing room.
"""
                (next Page57)

        Page2 c ->
            let
                ( deadCloneChoices, deadCloneCounters ) =
                    deadCloneChoicesAndCounters c
                        (\c_ ->
                            if c_.computerRequest then
                                Page45

                            else
                                Page32
                        )
            in
            textAndChoicesAndCounters
                """
"Greetings," says the kindly Internal Security self incrimination expert who
meets you at the door, "How are we doing today?"  He offers you a doughnut
and coffee and asks what brings you here.  This doesn't seem so bad, so you
tell him that you have come to confess some possible security lapses.  He
smiles knowingly, deftly catching your coffee as you slump to the floor.
"Nothing to be alarmed about it's just the truth serum," he says,
dragging you back into a discussion room.

The next five hours are a dim haze, but you can recall snatches of conversation
about your secret society, your mutant power, and your somewhat paranoid
distrust of The Computer.  This should explain why you are hogtied and moving
slowly down the conveyer belt towards the meat processing unit in Food
Services.
"""
                deadCloneChoices
                deadCloneCounters

        Page3 ->
            textAndChoices
                """
You walk to the nearest Computer terminal and request more information about
Christmas.  The Computer says, "That is an A-1 ULTRAVIOLET ONLY IMMEDIATE
TERMINATION classified topic.  What is your clearance please, Troubleshooter?"
"""
                [ ( "➡️ You give your correct clearance", Page4 )
                , ( "➡️ You lie and claim Ultraviolet clearance", Page5 )
                ]

        Page4 ->
            textAndChoices
                """
"That is classified information, Troubleshooter, thank you for your inquiry.
Please report to an Internal Security self incrimination station as soon as
possible."
"""
                (next Page9)

        Page5 ->
            textAndChoices
                """
The computer says, "Troubleshooter, you are not wearing the correct colour
uniform.  You must put on an Ultraviolet uniform immediately.  I have seen to
your needs and ordered one already it will be here shortly.  Please wait with
your back to the wall until it arrives."  In less than a minute an infrared
arrives carrying a white bundle.  He asks you to sign for it, then hands it to
you and stands back, well outside of a fragmentation grenade's blast radius.
"""
                [ ( "➡️ You open the package and put on the uniform", Page6 )
                , ( "➡️ You finally come to your senses and run for it", Page7 )
                ]

        Page6 ->
            textAndChoicesAndCounters
                """
The uniform definitely makes you look snappy and pert.  It really looks
impressive, and even has the new lopsided lapel fashion that you admire so
much.  What's more, citizens of all ranks come to obsequious attention as you
walk past.  This isn't so bad being an Ultraviolet.  You could probably come
to like it, given time.

The beeping computer terminal interrupts your musings.
"""
                (next Page8)
                { counters | ultraViolet = True }

        Page7 ->
            textAndChoicesAndCounters
                """
The corridor lights dim and are replaced by red battle lamps as the Security
Breach alarms howl all around you.  You run headlong down the corridor and
desperately windmill around a corner, only to collide with a squad of 12 Blue
clearance Vulture squadron soldiers.  "Stop, Slime Face," shouts the
commander, "or there won't be enough of you left for a tissue sample."
"All right, soldiers, stuff the greasy traitor into the uniform," he orders,
waving the business end of his blue laser scant inches from your nose.
With his other hand he shakes open a white bundle to reveal a pristine new
Ultraviolet citizen's uniform.

One of the Vulture squadron Troubleshooters grabs you by the neck in the
exotic and very painful Vulture Clamp(tm) death grip (you saw a special about
it on the Teela O'Malley show), while the rest tear off your clothes and
force you into the Ultraviolet uniform.  The moment you are dressed they step
clear and stand at attention.
"Thank you for your cooperation, sir," says the steely eyed leader of the
Vulture Squad.  "We will be going about our business now."  With perfect
timing the Vultures wheel smartly and goosestep down the corridor.

Special Note: don't make the mistake of assuming that your skills have
improved any because of the uniform you're only a Red Troubleshooter
traitorously posing as an Ultraviolet, and don't you forget it!

Suddenly, a computer terminal comes to life beside you.
"""
                (next Page8)
                { counters | ultraViolet = True }

        Page8 ->
            textAndChoices
                """
"Now, about your question, citizen.  Christmas was an old world marketing ploy
to induce lower clearance citizens to purchase vast quantities of goods, thus
accumulation a large amount of credit under the control of a single class of
citizen known as Retailers.  The strategy used is to imply that all good
citizens give gifts during Christmas, thus if one wishes to be a valuable
member of society one must also give gifts during Christmas.  More valuable
gifts make one a more valuable member, and thus did the Retailers come to
control a disproportionate amount of the currency.  In this way Christmas
eventually caused the collapse of the old world.  Understandably, Christmas
has been declared a treasonable practice in Alpha Complex.
Thank you for your inquiry."

You continue on your way to GDH7-beta.
"""
                (next Page10)

        Page9 ->
            textAndChoices
                """
As you walk toward the tubecar that will take you to GDH7-beta, you pass one
of the bright blue and orange Internal Security self incrimination stations.
Inside, you can see an IS agent cheerfully greet an infrared citizen and then
lead him at gunpoint into one of the rubber lined discussion rooms.
"""
                [ ( "➡️ You decide to stop here and chat, as ordered by The Computer", Page2 { counters | computerRequest = True } )
                , ( "➡️ You just continue blithely on past", Page10 )
                ]

        Page10 ->
            textAndChoices
                """
You stroll briskly down the corridor, up a ladder, across an unrailed catwalk,
under a perilously swinging blast door in urgent need of repair, and into
tubecar grand central.  This is the bustling hub of Alpha Complex tubecar
transportation.  Before you spreads a spaghetti maze of magnalift tube tracks
and linear accelerators.  You bravely study the specially enhanced 3-D tube
route map you wouldn't be the first Troubleshooter to take a fast tube ride
to nowhere.
"""
            <|
                (if counters.ultraViolet then
                    [ ( "➡️ You decide to ask The Computer about Christmas using a nearby terminal", Page3 ) ]

                 else
                    []
                )
                    ++ [ ( "➡️ You think you have the route worked out, so you'll board a tube train", Page10Tubecar ) ]

        Page10Tubecar ->
            ( model, Random.generate Page10TubecarResult (skillTestGen 2 10 counters.moxie) )

        Page10TubecarResult success ->
            let
                text =
                    """
You nervously select a tubecar and step aboard.

Let's see if you can roll under your moxie... {0}
"""
            in
            textAndChoices
                (interpolate text [ successText success ])
                (if success then
                    [ ( "➡️ You just caught a purple line tubecar.", Page13 ) ]

                 else
                    [ ( "➡️ You just caught a brown line tubecar.", Page48 ) ]
                )

        Page11 ->
            textAndChoices
                """
The printing on the folder says "Experimental Self Briefing."
You open it and begin to read the following:

Step 1: Compel the briefing subject to attend the briefing.
        Note: See Experimental Briefing Sub Form Indigo-WY-2,
        'Experimental Self Briefing Subject Acquisition Through The Use Of
        Neurotoxin Room Foggers.'

Step 2: Inform the briefing subject that the briefing has begun.
        ATTENTION: THE BRIEFING HAS BEGUN.

Step 3: Present the briefing material to the briefing subject.
        GREETINGS TROUBLESHOOTER.
        YOU HAVE BEEN SPECIALLY SELECTED TO SINGLEHANDEDLY
        WIPE OUT A DEN OF TRAITOROUS CHRISTMAS ACTIVITY.  YOUR MISSION IS TO
        GO TO GOODS DISTRIBUTION HALL 7-BETA AND ASSESS ANY CHRISTMAS ACTIVITY
        YOU FIND THERE.  YOU ARE TO INFILTRATE THESE CHRISTMAS CELEBRANTS,
        LOCATE THEIR RINGLEADER, AN UNKNOWN MASTER RETAILER, AND BRING HIM
        BACK FOR EXECUTION AND TRIAL.  THANK YOU.  THE COMPUTER IS YOUR FRIEND.

Step 4: Sign the briefing subject's briefing release form to indicate that
        the briefing subject has completed the briefing.
        ATTENTION: PLEASE SIGN YOUR BRIEFING RELEASE FORM.

Step 5: Terminate the briefing
        ATTENTION: THE BRIEFING IS TERMINATED.
"""
                (more Page11More)

        Page11More ->
            textAndChoices
                """
You walk to the door and hold your signed briefing release form up to the
plexiglass window.  A guard scrutinises it for a moment and then slides back
the megabolts holding the door shut.  You are now free to continue the
mission.
"""
                [ ( "➡️ You wish to ask The Computer for more information about Christmas", Page3 )
                , ( "➡️ You have decided to go directly to Goods Distribution Hall 7-beta", Page10 )
                ]

        Page12 ->
            textAndChoices
                """
You walk up to the door and push the button labelled "push to exit."
Within seconds a surly looking guard shoves his face into the small plexiglass
window.  You can see his mouth forming words but you can't hear any of them.
You just stare at him blankly for a few moments until he points down to a
speaker on your side of the door.  When you put your ear to it you can barely
hear him say, "Let's see your briefing release form, bud.  You aren't
getting out of here without it."
"""
                [ ( "➡️ You sit down at the table and read the Orange packet", Page11 )
                , ( "➡️ You stare around the room some more", Page57 )
                ]

        Page13 ->
            textAndChoices
                """
You step into the shiny plasteel tubecar, wondering why the shape has always
reminded you of bullets.  The car shoots forward the instant your feet touch
the slippery gray floor, pinning you immobile against the back wall as the
tubecar careens toward GDH7-beta.  Your only solace is the knowledge that it
could be worse, much worse.

Before too long the car comes to a stop.  You can see signs for GDH7-beta
through the window.  With a little practice you discover that you can crawl
to the door and pull open the latch.
"""
                (next Page14)

        Page14 ->
            textAndChoices
                """
You manage to pull yourself out of the tubecar and look around.  Before you is
one of the most confusing things you have ever seen, a hallway that is
simultaneously both red and green clearance.  If this is the result of
Christmas then it's easy to see the evils inherent in its practice.
You are in the heart of a large goods distribution centre.  You can see all
about you evidence of traitorous secret society Christmas celebration rubber
faced robots whiz back and forth selling toys to holiday shoppers, simul-plast
wreaths hang from every light fixture, while ahead in the shadows is a citizen
wearing a huge red synthetic flower.
"""
                (next Page22)

        Page15 ->
            textAndChoices
                """
You are set upon by a runty robot with a queer looking face and two pointy
rubber ears poking from beneath a tattered cap.  "Hey mister," it says,
"you done all your last minute Christmas shopping?  I got some real neat junk
here.  You don't wanna miss the big day tomorrow, if you know what I mean."
The robot opens its bag to show you a pile of shoddy Troubleshooter dolls.  It
reaches in and pulls out one of them.  "Look, these Action Troubleshooter(tm)
dolls are the neatest thing.  This one's got moveable arms and when you
squeeze him, his little rifle squirts realistic looking napalm.  It's only
50 credits.  Oh yeah, Merry Christmas."
"""
                [ ( "➡️ You decide to buy the doll.", Page16 )
                , ( "➡️ You shoot the robot.", Page17 )
                , ( "➡️ You ignore the robot and keep searching the hall.", Page22 )
                ]

        Page16 ->
            textAndChoicesAndCounters
                """
The doll is a good buy for fifty credits it will make a fine Christmas present
for one of your friends.  After the sale the robot rolls away.  You can use
the doll later in combat.  It works just like a cone rifle firing napalm,
except that occasionally it will explode and blow the user to smithereens.
But don't let that stop you.
"""
                (next Page22)
                { counters | actionDoll = True }

        Page17 ->
            textAndChoices
                """
You whip out your laser and shoot the robot, but not before it squeezes the
toy at you.  The squeeze toy has the same effect as a cone rifle firing napalm,
and the elfbot's armour has no effect against your laser.
"""
                [ ( "🔫 Fight!", Page17Fight 1 15 ) ]

        Page17Fight round enemyHP ->
            let
                hitGen : Int -> Int -> Int -> Int -> Random.Generator (Maybe Int)
                hitGen numOfHitRolls sides skill numOfDamageRolls =
                    skillTestGen numOfHitRolls sides skill
                        |> Random.andThen
                            (\hit ->
                                if hit then
                                    rollDice numOfDamageRolls sides |> Random.andThen (\damageRolls -> damageRolls |> Just |> Random.constant)

                                else
                                    Nothing |> Random.constant
                            )
            in
            ( model
            , Random.generate
                (\( maybePlayerDamage, maybeEnemyDamage ) ->
                    Page17FightResult round enemyHP maybePlayerDamage maybeEnemyDamage
                )
                (Random.pair (hitGen 1 100 25 1) (hitGen 2 100 40 2))
            )

        Page17FightResult round enemyHP maybePlayerDamage maybeEnemyDamage ->
            let
                heal : Counters -> ( String, Counters )
                heal c =
                    if c.hitPoints < 10 then
                        ( " after the GDH medbot has patched you up.", { c | hitPoints = 10 } )

                    else
                        ( ".", c )
            in
            if round <= 2 then
                let
                    ( text1, newCounters ) =
                        case maybePlayerDamage of
                            Just playerDamage ->
                                ( [ "You have been hit!" ], { counters | hitPoints = counters.hitPoints - playerDamage } )

                            Nothing ->
                                ( [ "It missed you, but not by much!" ], counters )

                    ( text2, choices, newerCounters ) =
                        if newCounters.hitPoints <= 0 then
                            let
                                ( deadCloneChoices, deadCloneCounters ) =
                                    deadCloneChoicesAndCounters newCounters (\_ -> Page45)
                            in
                            ( [], deadCloneChoices, deadCloneCounters )

                        else
                            case maybeEnemyDamage of
                                Just enemyDamage ->
                                    let
                                        newEnemyHP =
                                            enemyHP - enemyDamage
                                    in
                                    if newEnemyHP <= 0 then
                                        let
                                            ( lineEnd, healedCounters ) =
                                                heal newCounters
                                        in
                                        ( [ "You zapped the little bastard!", "You wasted it! Good shooting!", "", "You will need more evidence, so you search GDH7-beta further" ++ lineEnd ]
                                        , next Page22
                                        , healedCounters
                                        )

                                    else
                                        ( [ "You zapped the little bastard!" ]
                                        , next <| Page17Fight (round + 1) newEnemyHP
                                        , newCounters
                                        )

                                Nothing ->
                                    ( [ "Damn! You missed!" ]
                                    , next <| Page17Fight (round + 1) enemyHP
                                    , newCounters
                                    )
                in
                textAndChoicesAndCounters
                    ("" :: text1 ++ text2 |> String.join "\n")
                    choices
                    newerCounters

            else
                let
                    ( lineEnd, healedCounters ) =
                        heal counters
                in
                textAndChoicesAndCounters
                    ([ "", "It tried to fire again, but the toy exploded and demolished it.", "", "You will need more evidence, so you search GDH7-beta further" ++ lineEnd ] |> String.join "\n")
                    (next Page22)
                    healedCounters

        Page18 ->
            textAndChoices
                """
You walk to the centre of the hall, ogling like an infrared fresh from the
clone vats.  Towering before you is the most unearthly thing you have ever
seen, a green multi armed mutant horror hulking 15 feet above your head.
Its skeletal body is draped with hundreds of metallic strips (probably to
negate the effects of some insidious mutant power), and the entire hideous
creature is wrapped in a thousand blinking hazard lights.  It's times like
this when you wish you'd had some training for this job.  Luckily the
creature doesn't take notice of you but stands unmoving, as though waiting for
a summons from its dark lord, the Master Retailer.
"""
                (next Page18Balance)

        Page18Balance ->
            ( model, Random.generate Page18BalanceResult (skillTestGen 2 10 counters.agility) )

        Page18BalanceResult success ->
            let
                text =
                    """
WHAM, suddenly you are struck from behind.

Let's see if you can roll under your agility... {0}
"""
            in
            textAndChoices
                (interpolate text [ successText success ])
                (if success then
                    next Page19

                 else
                    next Page20
                )

        Page19 ->
            let
                newCounters =
                    { counters | killerCount = counters.killerCount + 1 }

                choices =
                    if newCounters.killerCount > counters.maxKill - counters.clone then
                        next Page19KilledTooMany

                    else if counters.readLetter then
                        next Page22

                    else
                        [ ( "You search the body, keeping an eye open for Internal Security", Page34 )
                        , ( "You run away like the cowardly dog you are", Page22 )
                        ]
            in
            textAndChoicesAndCounters
                """
Quickly you regain your balance, whirl and fire your laser into the Ultraviolet
citizen behind you.  For a moment your heart leaps to your throat, then you
realise that he is indeed dead and you will be the only one filing a report on
this incident.  Besides, he was participating in this traitorous Christmas
shopping, as is evident from the rain of shoddy toys falling all around you.

Another valorous deed done in the service of The Computer!
"""
                choices
                newCounters

        Page19KilledTooMany ->
            let
                ( choices, newCounters ) =
                    deadCloneChoicesAndCounters counters (\_ -> Page45)
            in
            textAndChoicesAndCounters
                """
You have been wasting the leading citizens of Alpha Complex at a prodigious
rate.  This has not gone unnoticed by the Internal Security squad at GDH7-beta.
Suddenly, a net of laser beams spear out of the gloomy corners of the hall,
chopping you into teeny, weeny bite size pieces.
"""
                choices
                newCounters

        Page20 ->
            textAndChoices
                """
Oh no! you can't keep your balance.  You're falling, falling head first into
the Christmas beast's gaping maw.  It's a valiant struggle you think you are
gone when its poisonous needles dig into your flesh, but with a heroic effort
you jerk a string of lights free and jam the live wires into the creature's
spine.  The Christmas beast topples to the ground and begins to burn, filling
the area with a thick acrid smoke.  It takes only a moment to compose yourself,
and then you are ready to continue your search for the Master Retailer.
"""
                (next Page22)

        Page21 ->
            textAndChoices
                """
Suddenly a large florescent sign pops up from the ground. It reads "SCENARIO
THIS WAY" and points off between two rows of caroling elfbots.
"""
                [ ( "➡️ Follow the sign", Page29 )
                , ( "➡️ Ignore the sign", Page22 )
                ]

        Page22 ->
            ( model, Random.generate Page22RandomEncounterResult <| Random.uniform Page18 [ Page15, Page21, Page29 ] )

        Page22RandomEncounterResult nextChoice ->
            textAndChoices
                """
You are searching Goods Distribution Hall 7-beta.
"""
                (next nextChoice)

        Page23 ->
            textAndChoices
                """
You go to the nearest computer terminal and declare yourself a mutant.
"A mutant, he's a mutant," yells a previously unnoticed infrared who had
been looking over your shoulder.  You easily gun him down, but not before a
dozen more citizens take notice and aim their weapons at you.
"""
                [ ( "➡️ You tell them that it was really only a bad joke", Page28 )
                , ( "➡️ You want to fight it out, one against twelve", Page24 )
                ]

        Page24 ->
            textAndChoices
                """
Golly, I never expected someone to pick this.  I haven't even designed
the 12 citizens who are going to make a sponge out of you.  Tell you what,
I'll give you a second chance.
"""
                [ ( "➡️ You change your mind and say it was only a bad joke", Page28 )
                , ( "➡️ You REALLY want to shoot it out", Page25 )
                ]

        Page25 ->
            textAndChoices
                """
Boy, you really can't take a hint!
They're closing in.  Their trigger fingers are twitching, they're about to
shoot.  This is your last chance.
"""
                [ ( "➡️ You tell them it was all just a bad joke", Page28 )
                , ( "➡️ You are going to shoot", Page26 )
                ]

        Page26 ->
            let
                ( choices, newCounters ) =
                    deadCloneChoicesAndCounters counters (\_ -> Page32)
            in
            textAndChoicesAndCounters
                """
You can read the cold, sober hatred in their eyes (They really didn't think
it was funny), as they tighten the circle around you.  One of them shoves a
blaster up your nose, but that doesn't hurt as much as the multi-gigawatt
carbonium tipped food drill in the small of your back.
You spend the remaining micro-seconds of your life wondering what you did wrong
"""
                choices
                newCounters

        Page28 ->
            textAndChoices
                """
They don't think it's funny.
"""
                (next Page26)

        Page29 ->
            textAndChoices
                """
"Psst, hey citizen, come here.  Pssfft," you hear.  When you peer around
you can see someone's dim outline in the shadows.  "I got some information
on the Master Retailer.  It'll only cost you 30 psst credits."
"""
                [ ( "➡️ You pay the 30 credits for the info.", Page30 )
                , ( "➡️ You would rather threaten him for the information.", Page31 )
                , ( "➡️ You ignore him and walk away.", Page22 )
                ]

        Page30 ->
            textAndChoices
                """
You step into the shadows and offer the man a thirty credit bill.  "Just drop
it on the floor," he says.  "So you're looking for the Master Retailer, pssfft?
I've seen him, he's a fat man in a fuzzy red and white jump suit.  They say
he's a high programmer with no respect for proper security.  If you want to
find him then pssfft step behind me and go through the door."
Behind the man is a reinforced plasteel blast door.  The centre of it has been
buckled toward you in a manner you only saw once before when you were field
testing the rocket assist plasma slingshot (you found it easily portable but
prone to misfire).  Luckily it isn't buckled too far for you to make out the
warning sign.  WARNING!! Don't open this door or the same thing will happen to
you.  Opening this door is a capital offense.  Do not do it.  Not at all. This
is not a joke.
"""
                [ ( "➡️ You use your Precognition mutant power on opening the door.", Page56 )
                , ( "➡️ You just go through the door anyway.", Page33 )
                , ( "➡️ You decide it's too dangerous and walk away.", Page22 )
                ]

        Page31 ->
            textAndChoices
                """
Like any good troubleshooter you make the least expensive decision and threaten
him for information.  With lightning like reflexes you whip out your laser and
stick it up his nose.  "Talk, you traitorous Christmas celebrator, or who nose
what will happen to you, yuk yuk," you pun menacingly, and then you notice
something is very wrong.  He doesn't have a nose.  As a matter of fact he's
made of one eighth inch cardboard and your laser is sticking through the other
side of his head.  "Are you going to pay?" says his mouth speaker,
"or are you going to pssfft go away stupid?"
"""
                [ ( "➡️ You pay the 30 credits", Page30 )
                , ( "➡️ You pssfft go away stupid", Page22 )
                ]

        Page32 ->
            textAndChoices
                """
Finally it's your big chance to prove that you're as good a troubleshooter
as your previous clone.  You walk briskly to mission briefing and pick up your
previous clone's personal effects and notepad.  After reviewing the notes you
know what has to be done.  You catch the purple line to Goods Distribution Hall
7-beta and begin to search for the blast door.
"""
                (next Page22)

        Page33 ->
            textAndChoicesAndCounters
                """
You release the megabolts on the blast door, then strain against it with your
awesome strength.  Slowly the door creaks open.  You bravely leap through the
opening and smack your head into the barrel of a 300 mm 'ultra shock' class
plasma cannon.  It's dark in the barrel now, but just before your head got
stuck you can remember seeing a group of technicians anxiously watch you leap
into the room.
"""
                (if counters.ultraViolet then
                    next Page35

                 else
                    next Page36
                )
                { counters | blastDoor = True }

        Page34 ->
            textAndChoices
                """
You have found a sealed envelope on the body.  You open it and read:
"WARNING: Ultraviolet Clearance ONLY.  DO NOT READ.

Memo from Chico-U-MRX4 to Harpo-U-MRX5.

The planned takeover of the Troubleshooter Training Course goes well, Comrade.
Once we have trained the unwitting bourgeois troubleshooters to work as
communist dupes, the overthrow of Alpha Complex will be unstoppable.  My survey
of the complex has convinced me that no one suspects a thing soon it will be
too late for them to oppose the revolution.  The only thing that could possibly
impede the people's revolution would be someone alerting The Computer to our
plans (for instance, some enterprising Troubleshooter could tell The Computer
that the communists have liberated the Troubleshooter Training Course and plan
to use it as a jumping off point from which to undermine the stability of all
Alpha Complex), but as we both know, the capitalistic Troubleshooters would
never serve the interests of the proletariat above their own bourgeois desires.

P.S. I'm doing some Christmas shopping later today.  Would you like me to pick
you up something?"
"""
                (more Page34More)

        Page34More ->
            textAndChoicesAndCounters
                """
When you put down the memo you are overcome by that strange deja vu again.
You see yourself talking privately with The Computer.  You are telling it all
about the communists' plan, and then the scene shifts and you see yourself
showered with awards for foiling the insidious communist plot to take over the
complex.
"""
                [ ( "➡️ You rush off to the nearest computer terminal to expose the commies", Page46 )
                , ( "➡️ You wander off to look for more evidence", Page22 )
                ]
                { counters | readLetter = True }

        Page35 ->
            let
                ( choices, newCounters ) =
                    deadCloneChoicesAndCounters counters (\_ -> Page32)
            in
            textAndChoicesAndCounters
                """
"Oh master," you hear through the gun barrel, "where have you been? It is
time for the great Christmas gifting ceremony.  You had better hurry and get
the costume on or the trainee may begin to suspect."  For the second time
today you are forced to wear attire not of your own choosing.  They zip the
suit to your chin just as you hear gunfire erupt behind you.
"Oh no! Who left the door open?  The commies will get in.  Quick, fire the
laser cannon or we're all doomed."

"Too late you capitalist swine, the people's revolutionary strike force claims
this cannon for the proletariat's valiant struggle against oppression.  Take
that, you running dog imperialist lackey.  ZAP, KAPOW"

Just when you think that things couldn't get worse, "Aha, look what we have
here, the Master Retailer himself with his head caught in his own cannon.  His
death will serve as a symbol of freedom for all Alpha Complex.
Fire the cannon."
"""
                choices
                newCounters

        Page36 ->
            textAndChoices
                """
"Congratulations, troubleshooter, you have successfully found the lair of the
Master Retailer and completed the Troubleshooter Training Course test mission,"
a muffled voice tells you through the barrel.  "Once we dislodge your head
from the barrel of the 'Ultra Shock' plasma cannon you can begin with the
training seminars, the first of which will concern the 100% accurate
identification and elimination of unregistered mutants.  If you have any
objections please voice them now."
"""
                [ ( "➡️ You appreciate his courtesy and voice an objection.", Page32 )
                , ( "➡️ After your head is removed from the cannon, you register as a mutant.", Page23 )
                , ( "➡️ After your head is removed from the cannon, you go to the unregistered mutant identification and elimination seminar.", Page37 )
                ]

        Page37 ->
            textAndChoices
                """
"Come with me please, Troubleshooter," says the Green clearance technician
after he has dislodged your head from the cannon.  "You have been participating
in the Troubleshooter Training Course since you got off the tube car in
GDH7-beta," he explains as he leads you down a corridor.  "The entire
Christmas assignment was a test mission to assess your current level of
training.  You didn't do so well.  We're going to start at the beginning with
the other student.  Ah, here we are, the mutant identification and elimination
lecture."  He shows you into a vast lecture hall filled with empty seats.
There is only one other student here, a Troubleshooter near the front row
playing with his Action Troubleshooter(tm) figure.  "Find a seat and I will
begin," says the instructor.
"""
                (next Page38)

        Page38 ->
            let
                text =
                    """
"I am Plato-B-PHI {0}, head of mutant propaganda here at the training
course.

If you have any questions about mutants please come to me.  Today I will be
talking about mutant detection.  Detecting mutants is very easy.  One simply
watches for certain tell tale signs, such as the green scaly skin, the third
arm growing from the forehead, or other similar disfigurements so common with
their kind.  There are, however, a few rare specimens that show no outward sign
of their treason.  This has been a significant problem, so our researchers have
been working on a solution.  I would like a volunteer to test this device,"
he says, holding up a ray gun looking thing.  "It is a mutant detection ray.
This little button detects for mutants, and this big button stuns them once
they are discovered.  Who would like to volunteer for a test?"

The Troubleshooter down the front squirms deeper into his chair.
"""
            in
            textAndChoices
                (interpolate text [ String.fromInt counters.platoClone ])
                [ ( "➡️ You volunteer for the test", Page39 )
                , ( "➡️ You duck behind a chair and hope the instructor doesn't notice you", Page40 )
                ]

        Page39 ->
            let
                text =
                    """
You bravely volunteer to test the mutant detection gun.  You stand up and walk
down the steps to the podium, passing a very relieved Troubleshooter along the
way. When you reach the podium Plato-B-PHI {0} hands you the mutant detection gun
and says, "Here, aim the gun at that Troubleshooter and push the small button.
If you see a purple light, stun him."  Grasping the opportunity to prove your
worth to The Computer, you fire the mutant detection ray at the Troubleshooter.
A brilliant purple nimbus instantly surrounds his body.  You slip your finger
to the large stun button and he falls writhing to the floor.

"Good shot," says the instructor as you hand him the mutant detection gun,
"I'll see that you get a commendation for this.  It seems you have the hang
of mutant detection and elimination.  You can go on to the secret society
infiltration class.  I'll see that the little mutie gets packaged for
tomorrow's mutant dissection class."
"""
            in
            textAndChoices
                (interpolate text [ String.fromInt counters.platoClone ])
                (next Page41)

        Page40 ->
            let
                text =
                    """
You breathe a sigh of relief as Plato-B-PHI {0} picks on the other Troubleshooter.
"You down here in the front," says the instructor pointing at the other
Troubleshooter, "you'll make a good volunteer.  Please step forward."
The Troubleshooter looks around with a `who me?' expression on his face, but
since he is the only one visible in the audience he figures his number is up.
He walks down to the podium clutching his Action Troubleshooter(tm) doll before
him like a weapon.  "Here," says Plato-B-PHI {0}, "take the mutant detection ray
and point it at the audience.  If there are any mutants out there we'll know
soon enough."  Suddenly your skin prickles with static electricity as a bright
purple nimbus surrounds your body.  "Ha Ha, got one," says the instructor.
"Stun him before he gets away."
"""
            in
            textAndChoices
                (interpolate text [ String.fromInt counters.platoClone ])
                [ ( "🔫 Fight!", Page40Fight ) ]

        Page40Fight ->
            let
                genHit : Int -> Random.Generator Bool
                genHit skill =
                    Random.int 1 100
                        |> Random.andThen (\hitRoll -> hitRoll <= skill |> Random.constant)
            in
            ( model
            , Random.generate
                (\( playerHit, enemyHit ) ->
                    Page40FightResult playerHit enemyHit
                )
                (Random.pair (genHit 30) (genHit 40))
            )

        Page40FightResult playerHit enemyHit ->
            let
                ( text, choices ) =
                    if playerHit then
                        ( "His shot hits you.  You feel numb all over.", next Page49 )

                    else if enemyHit then
                        ( "His shot just missed.\n\nYou just blew his head off.  His lifeless hand drops the mutant detector ray.", next Page50 )

                    else
                        ( "His shot just missed.\n\nYou burnt a hole in the podium.  He sights the mutant detector ray on you.", next Page40Fight )
            in
            textAndChoices text choices

        Page41 ->
            textAndChoices
                """
You stumble down the hallway of the Troubleshooter Training Course looking for
your next class.  Up ahead you see one of the instructors waving to you.  When
you get there he shakes your hand and says, "I am Jung-I-PSY.  Welcome to the
secret society infiltration seminar.  I hope you ..."  You don't catch the
rest of his greeting because you're paying too much attention to his handshake
it is the strangest thing that has ever been done to your hand, sort of how it
would feel if you put a neuro whip in a high energy palm massage unit.

It doesn't take you long to learn what he is up to you feel him briefly shake
your hand with the secret Illuminati handshake.
"""
                [ ( "➡️ You respond with the proper Illuminati code phrase, \"Ewige Blumenkraft\"", Page42 )
                , ( "➡️ You ignore this secret society contact", Page43 )
                ]

        Page42 ->
            textAndChoices
                """
"Aha, so you are a member of the elitist Illuminati secret society," he says
loudly, "that is most interesting."  He turns to the large class already
seated in the auditorium and says, "You see, class, by simply using the correct
hand shake you can identify the member of any secret society.  Please keep your
weapons trained on him while I call a guard.
"""
                [ ( "➡️ You run for it", Page51 )
                , ( "➡️ You wait for the guard", Page52 )
                ]

        Page43 ->
            textAndChoices
                """
You sit through a long lecture on how to recognise and infiltrate secret
societies, with an emphasis on mimicking secret handshakes.  The basic theory,
which you realise to be sound from your Iluminati training, is that with the
proper handshake you can pass unnoticed in any secret society gathering.
What's more, the proper handshake will open doors faster than an 'ultra shock'
plasma cannon.  You are certain that with the information you learn here you
will easily be promoted to the next level of your Illuminati secret society.
The lecture continues for three hours, during which you have the opportunity
to practice many different handshakes.  Afterwards everyone is directed to
attend the graduation ceremony.  Before you must go you have a little time to
talk to The Computer about, you know, certain topics.
"""
                [ ( "➡️ You go looking for a computer terminal", Page44 )
                , ( "➡️ You go to the graduation ceremony immediately", Page55 )
                ]

        Page44 ->
            textAndChoices
                """
You walk down to a semi-secluded part of the training course complex and
activate a computer terminal.  "AT YOUR SERVICE" reads the computer screen.
"""
            <|
                if counters.readLetter then
                    [ ( "➡️ You register yourself as a mutant.", Page23 )
                    , ( "➡️ You want to chat about the commies.", Page46 )
                    , ( "➡️ You change your mind and go to the graduation ceremony.", Page55 )
                    ]

                else
                    [ ( "➡️ You register yourself as a mutant.", Page23 )
                    , ( "➡️ You change your mind and go to the graduation ceremony.", Page55 )
                    ]

        Page45 ->
            textAndChoices
                """
"Hrank Hrank," snorts the alarm in your living quarters.  Something is up.
You look at the monitor above the bathroom mirror and see the message you have
been waiting for all these years.  "ATTENTION TROUBLESHOOTER, YOU ARE BEING
ACTIVATED. PLEASE REPORT IMMEDIATELY TO MISSION ASSIGNMENT ROOM A17/GAMMA/LB22.
THANK YOU. THE COMPUTER IS YOUR FRIEND."  When you arrive at mission
assignment room A17-gamma/LB22 you are given your previous clone's
remaining possessions and notebook.  You puzzle through your predecessor's
cryptic notes, managing to decipher enough to lead you to the tube station and
the tube car to GDH7-beta.
"""
                (next Page10)

        Page46 ->
            textAndChoices
                """
"Why do you ask about the communists, Troubleshooter?  It is not in the
interest of your continued survival to be asking about such topics," says
The Computer.
"""
                [ ( "➡️ You insist on talking about the communists", Page53 )
                , ( "➡️ You change the subject", Page54 )
                ]

        Page47 ->
            textAndChoices
                """
The Computer orders the entire Vulture squadron to terminate the Troubleshooter
Training Course.  Unfortunately you too are terminated for possessing
classified information.

Don't act so innocent, we both know that you are an Illuminatus which is in
itself an act of treason.

Don't look to me for sympathy.

THE END
"""
                restart

        Page48 ->
            let
                ( choices, newCounters ) =
                    deadCloneChoicesAndCounters counters (\_ -> Page45)
            in
            textAndChoicesAndCounters
                """
The tubecar shoots forward as you enter, slamming you back into a pile of
garbage.  The front end rotates upward and you, the garbage and the garbage
disposal car shoot straight up out of Alpha Complex.  One of the last things
you see is a small blue sphere slowly dwindling behind you.  After you fail to
report in, you will be assumed dead.
"""
                choices
                newCounters

        Page49 ->
            let
                ( choices, newCounters ) =
                    deadCloneChoicesAndCounters counters (\_ -> Page32)
            in
            textAndChoicesAndCounters
                """
The instructor drags your inert body into a specimen detainment cage.
"He'll make a good subject for tomorrow's mutant dissection class," you hear.
"""
                choices
                newCounters

        Page50 ->
            textAndChoicesAndCounters
                """
You put down the other Troubleshooter, and then wisely decide to drill a few
holes in the instructor as well the only good witness is a dead witness.
You continue with the training course.
"""
                (next Page41)
                { counters | platoClone = counters.platoClone + 1 }

        Page51 ->
            let
                ( choices, newCounters ) =
                    deadCloneChoicesAndCounters counters (\_ -> Page32)
            in
            textAndChoicesAndCounters
                """
You run for it, but you don't run far.  Three hundred strange and exotic
weapons turn you into a freeze dried cloud of soot.
"""
                choices
                newCounters

        Page52 ->
            textAndChoices
                """
You wisely wait until the instructor returns with a Blue Internal Security
guard.  The guard leads you to an Internal Security self incrimination station.
"""
                (next <| Page2 counters)

        Page53 ->
            textAndChoices
                """
You tell The Computer about:
"""
                [ ( "➡️ The commies who have infiltrated the Troubleshooter Training Course and the impending People's Revolution", Page47 )
                , ( "➡️ Something less dangerous", Page54 )
                ]

        Page54 ->
            let
                ( deadCloneChoices, deadCloneCounters ) =
                    deadCloneChoicesAndCounters counters
                        (\c_ ->
                            if c_.blastDoor then
                                Page45

                            else
                                Page32
                        )
            in
            textAndChoicesAndCounters
                """
"Do not try to change the subject, Troubleshooter," says The Computer.
"It is a serious crime to ask about the communists.  You will be terminated
immediately.  Thank you for your inquiry.  The Computer is your friend."

Steel bars drop to your left and right, trapping you here in the hallway.
A spotlight beams from the computer console to brilliantly iiluminate you while
the speaker above your head rapidly repeats "Traitor, Traitor, Traitor."
It doesn't take long for a few guards to notice your predicament and come to
finish you off.
"""
                deadCloneChoices
                deadCloneCounters

        Page55 ->
            textAndChoices
                """
You and 300 other excited graduates are marched from the lecture hall and into
a large auditorium for the graduation exercise.  The auditorium is
extravagantly decorated in the colours of the graduating class.  Great red and
green plasti-paper ribbons drape from the walls, while a huge sign reading
"Congratulations class of GDH7-beta-203.44/A" hangs from the raised stage down
front.  Once everyone finds a seat the ceremony begins.  Jung-I-PSY is the
first to speak, "Congratulations students, you have successfully survived the
Troubleshooter Training Course.  It always brings me great pride to address
the graduating class, for I know, as I am sure you do too, that you are now
qualified for the most perilous missions The Computer may select for you.  The
thanks is not owed to us of the teaching staff, but to all of you, who have
persevered and graduated.  Good luck and die trying."  Then the instructor
begins reading the names of the students who one by one walk to the front of
the auditorium and receive their diplomas.
"""
                (more Page55More)

        Page55More ->
            let
                text =
                    """
Soon it is your turn, "Philo-R-DMD, graduating a master of mutant
identification and secret society infiltration."  You walk up and receive your
diploma from Plato-B-PHI {0}, then return to your seat.  There is another speech
after the diplomas are handed out, but it is cut short by by rapid
fire laser bursts from the high spirited graduating class.  You are free to
return to your barracks to wait, trained and fully qualified, for your next
mission.  You also get that cherished promotion from the Illuminati secret
society.  In a week you receive a detailed Training Course bill totalling
1,523 credits.

THE END
"""
            in
            textAndChoices
                (interpolate text [ String.fromInt counters.platoClone ])
                restart

        Page56 ->
            textAndChoices
                """
That familiar strange feeling of deja vu envelops you again.  It is hard to
say, but whatever is on the other side of the door does not seem to be intended
for you.
"""
                [ ( "➡️ You open the door and step through", Page33 )
                , ( "➡️ You go looking for more information", Page22 )
                ]

        Page57 ->
            textAndChoices
                """
In the centre of the room is a table and a single chair.  There is an Orange
folder on the table top, but you can't make out the lettering on it.
"""
                [ ( "➡️ You sit down and read the folder", Page11 )
                , ( "➡️ You leave the room", Page12 )
                ]

        YouLose ->
            textAndChoicesAndCounters
                """
*** You Lose ***

All your clones are dead.  Your name has been stricken from the records.

THE END
"""
                restart
                initCounters

        ToggleCharSheet ->
            ( { model | showCharSheet = not model.showCharSheet }, Cmd.none )


initCounters =
    { moxie = 13
    , agility = 15
    , hitPoints = 10
    , clone = 1
    , killerCount = 0
    , maxKill = 7
    , platoClone = 3
    , computerRequest = False
    , ultraViolet = False
    , actionDoll = False
    , readLetter = False
    , blastDoor = False
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( instructionsModel, Cmd.none )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


preFormattedElement : String -> Element msg
preFormattedElement text =
    paragraph
        [ Font.family [ Font.monospace ]
        , Element.htmlAttribute (style "white-space" "pre")
        ]
        [ Element.html
            (Html.text text)
        ]


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.textColumn [ spacingXY 10 30, padding 10 ] <|
            let
                ( mainText, choices ) =
                    if model.showCharSheet then
                        ( charSheet model.counters, [ ( "⬅️ Back to game", ToggleCharSheet ) ] )

                    else
                        ( model.text, model.choices ++ [ ( "📊 Show character sheet", ToggleCharSheet ) ] )
            in
            let
                otherParas =
                    List.map
                        (\( linkText, msg ) ->
                            paragraph []
                                [ button []
                                    { onPress = Just <| msg
                                    , label = Element.text linkText
                                    }
                                ]
                        )
                    <|
                        choices
            in
            paragraph []
                [ preFormattedElement <| mainText ]
                :: otherParas
