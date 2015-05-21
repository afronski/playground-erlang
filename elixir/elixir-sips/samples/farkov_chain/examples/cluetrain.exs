defmodule Cluetrain do
  alias FarkovChain.Dictionary
  alias FarkovChain.Generator

  def run do
    :random.seed(:erlang.now())
    Dictionary.new
    |> Dictionary.parse(manifesto)
    |> Generator.generate_words("powerful", 100)
  end

  def manifesto do
    """
    A powerful global conversation has begun. Through the Internet, people are discovering and inventing new ways to share relevant knowledge with blinding speed. As a direct result, markets are getting smarter—and getting smarter faster than most companies.
    These markets are conversations. Their members communicate in language that is natural, open, honest, direct, funny and often shocking. Whether explaining or complaining, joking or serious, the human voice is unmistakably genuine. It can't be faked.

    roadkill

    Most corporations, on the other hand, only know how to talk in the soothing, humorless monotone of the mission statement, marketing brochure, and your-call-is-important-to-us busy signal. Same old tone, same old lies. No wonder networked markets have no respect for companies unable or unwilling to speak as they do.

    But learning to speak in a human voice is not some trick, nor will corporations convince us they are human with lip service about "listening to customers." They will only sound human when they empower real human beings to speak on their behalf.

    While many such people already work for companies today, most companies ignore their ability to deliver genuine knowledge, opting instead to crank out sterile happytalk that insults the intelligence of markets literally too smart to buy it.

    However, employees are getting hyperlinked even as markets are. Companies need to listen carefully to both. Mostly, they need to get out of the way so intranetworked employees can converse directly with internetworked markets.

    Corporate firewalls have kept smart employees in and smart markets out. It's going to cause real pain to tear those walls down. But the result will be a new kind of conversation. And it will be the most exciting conversation business has ever engaged in.

    if you only have time for one clue this year, this is the one to get... 

    Online Markets...

    Networked markets are beginning to self-organize faster than the companies that have traditionally served them. Thanks to the web, markets are becoming better informed, smarter, and more demanding of qualities missing from most business organizations.

    ...People of Earth

    The sky is open to the stars. Clouds roll over us night and day. Oceans rise and fall. Whatever you may have heard, this is our world, our place to be. Whatever you've been told, our flags fly free. Our heart goes on forever. People of Earth, remember.

    95 Theses Markets are conversations.  Markets consist of human beings, not
    demographic sectors.  Conversations among human beings sound human. They are
    conducted in a human voice.  Whether delivering information, opinions,
    perspectives, dissenting arguments or humorous asides, the human voice is
    typically open, natural, uncontrived.  People recognize each other as such
    from the sound of this voice.  The Internet is enabling conversations among
    human beings that were simply not possible in the era of mass media.
    Hyperlinks subvert hierarchy.  In both internetworked markets and among
    intranetworked employees, people are speaking to each other in a powerful
    new way.  These networked conversations are enabling powerful new forms of
    social organization and knowledge exchange to emerge.  As a result, markets
    are getting smarter, more informed, more organized. Participation in a
    networked market changes people fundamentally.  People in networked markets
    have figured out that they get far better information and support from one
    another than from vendors. So much for corporate rhetoric about adding value
    to commoditized products.  There are no secrets. The networked market knows
    more than companies do about their own products. And whether the news is
    good or bad, they tell everyone.  What's happening to markets is also
    happening among employees. A metaphysical construct called "The Company" is
    the only thing standing between the two.  Corporations do not speak in the
    same voice as these new networked conversations. To their intended online
    audiences, companies sound hollow, flat, literally inhuman.  In just a few
    more years, the current homogenized "voice" of business—the sound of mission
    statements and brochures—will seem as contrived and artificial as the
    language of the 18th century French court.  Already, companies that speak in
    the language of the pitch, the dog-and-pony show, are no longer speaking to
    anyone.  Companies that assume online markets are the same markets that used
    to watch their ads on television are kidding themselves.  Companies that
    don't realize their markets are now networked person-to-person, getting
    smarter as a result and deeply joined in conversation are missing their best
    opportunity.  Companies can now communicate with their markets directly. If
    they blow it, it could be their last chance.  Companies need to realize
    their markets are often laughing. At them.  Companies need to lighten up and
    take themselves less seriously. They need to get a sense of humor.  Getting
    a sense of humor does not mean putting some jokes on the corporate web
    site. Rather, it requires big values, a little humility, straight talk, and
    a genuine point of view.  Companies attempting to "position" themselves need
    to take a position. Optimally, it should relate to something their market
    actually cares about.  Bombastic boasts—"We are positioned to become the
    preeminent provider of XYZ"—do not constitute a position.  Companies need to
    come down from their Ivory Towers and talk to the people with whom they hope
    to create relationships.  Public Relations does not relate to the
    public. Companies are deeply afraid of their markets.  By speaking in
    language that is distant, uninviting, arrogant, they build walls to keep
    markets at bay.  Most marketing programs are based on the fear that the
    market might see what's really going on inside the company.  Elvis said it
    best: "We can't go on together with suspicious minds."  Brand loyalty is the
    corporate version of going steady, but the breakup is inevitable—and coming
    fast. Because they are networked, smart markets are able to renegotiate
    relationships with blinding speed.  Networked markets can change suppliers
    overnight. Networked knowledge workers can change employers over lunch. Your
    own "downsizing initiatives" taught us to ask the question: "Loyalty? What's
    that?"  Smart markets will find suppliers who speak their own language.
    Learning to speak with a human voice is not a parlor trick. It can't be
    "picked up" at some tony conference.  To speak with a human voice, companies
    must share the concerns of their communities.  But first, they must belong
    to a community.  Companies must ask themselves where their corporate
    cultures end.  If their cultures end before the community begins, they will
    have no market.  Human communities are based on discourse—on human speech
    about human concerns.  The community of discourse is the market.  Companies
    that do not belong to a community of discourse will die.  Companies make a
    religion of security, but this is largely a red herring. Most are protecting
    less against competitors than against their own market and workforce.  As
    with networked markets, people are also talking to each other directly
    inside the company—and not just about rules and regulations, boardroom
    directives, bottom lines.  Such conversations are taking place today on
    corporate intranets. But only when the conditions are right.  Companies
    typically install intranets top-down to distribute HR policies and other
    corporate information that workers are doing their best to ignore.
    Intranets naturally tend to route around boredom. The best are built
    bottom-up by engaged individuals cooperating to construct something far more
    valuable: an intranetworked corporate conversation.  A healthy intranet
    organizes workers in many meanings of the word. Its effect is more radical
    than the agenda of any union.  While this scares companies witless, they
    also depend heavily on open intranets to generate and share critical
    knowledge. They need to resist the urge to "improve" or control these
    networked conversations.  When corporate intranets are not constrained by
    fear and legalistic rules, the type of conversation they encourage sounds
    remarkably like the conversation of the networked marketplace.  Org charts
    worked in an older economy where plans could be fully understood from atop
    steep management pyramids and detailed work orders could be handed down from
    on high.  Today, the org chart is hyperlinked, not hierarchical. Respect for
    hands-on knowledge wins over respect for abstract authority.
    Command-and-control management styles both derive from and reinforce
    bureaucracy, power tripping and an overall culture of paranoia.  Paranoia
    kills conversation. That's its point. But lack of open conversation kills
    companies.  There are two conversations going on. One inside the
    company. One with the market.  In most cases, neither conversation is going
    very well. Almost invariably, the cause of failure can be traced to obsolete
    notions of command and control.  As policy, these notions are poisonous. As
    tools, they are broken. Command and control are met with hostility by
    intranetworked knowledge workers and generate distrust in internetworked
    markets.  These two conversations want to talk to each other. They are
    speaking the same language. They recognize each other's voices.  Smart
    companies will get out of the way and help the inevitable to happen sooner.
    If willingness to get out of the way is taken as a measure of IQ, then very
    few companies have yet wised up.  However subliminally at the moment,
    millions of people now online perceive companies as little more than quaint
    legal fictions that are actively preventing these conversations from
    intersecting.  This is suicidal. Markets want to talk to companies.  Sadly,
    the part of the company a networked market wants to talk to is usually
    hidden behind a smokescreen of hucksterism, of language that rings false—and
    often is.  Markets do not want to talk to flacks and hucksters. They want to
    participate in the conversations going on behind the corporate firewall.
    De-cloaking, getting personal: We are those markets. We want to talk to you.
    We want access to your corporate information, to your plans and strategies,
    your best thinking, your genuine knowledge. We will not settle for the
    4-color brochure, for web sites chock-a-block with eye candy but lacking any
    substance.  We're also the workers who make your companies go. We want to
    talk to customers directly in our own voices, not in platitudes written into
    a script.  As markets, as workers, both of us are sick to death of getting
    our information by remote control. Why do we need faceless annual reports
    and third-hand market research studies to introduce us to each other?  As
    markets, as workers, we wonder why you're not listening. You seem to be
    speaking a different language.  The inflated self-important jargon you sling
    around—in the press, at your conferences—what's that got to do with us?
    Maybe you're impressing your investors. Maybe you're impressing Wall
    Street. You're not impressing us.  If you don't impress us, your investors
    are going to take a bath. Don't they understand this? If they did, they
    wouldn't let you talk that way.  Your tired notions of "the market" make our
    eyes glaze over. We don't recognize ourselves in your projections—perhaps
    because we know we're already elsewhere.  We like this new marketplace much
    better. In fact, we are creating it.  You're invited, but it's our
    world. Take your shoes off at the door. If you want to barter with us, get
    down off that camel!  We are immune to advertising. Just forget it.  If you
    want us to talk to you, tell us something. Make it something interesting for
    a change.  We've got some ideas for you too: some new tools we need, some
    better service. Stuff we'd be willing to pay for. Got a minute?  You're too
    busy "doing business" to answer our email? Oh gosh, sorry, gee, we'll come
    back later. Maybe.  You want us to pay? We want you to pay attention.  We
    want you to drop your trip, come out of your neurotic self-involvement, join
    the party.  Don't worry, you can still make money. That is, as long as it's
    not the only thing on your mind.  Have you noticed that, in itself, money is
    kind of one-dimensional and boring? What else can we talk about?  Your
    product broke. Why? We'd like to ask the guy who made it. Your corporate
    strategy makes no sense. We'd like to have a chat with your CEO. What do you
    mean she's not in?  We want you to take 50 million of us as seriously as you
    take one reporter from The Wall Street Journal.  We know some people from
    your company. They're pretty cool online. Do you have any more like that
    you're hiding? Can they come out and play?  When we have questions we turn
    to each other for answers. If you didn't have such a tight rein on "your
    people" maybe they'd be among the people we'd turn to.  When we're not busy
    being your "target market," many of us are your people. We'd rather be
    talking to friends online than watching the clock. That would get your name
    around better than your entire million dollar web site. But you tell us
    speaking to the market is Marketing's job.  We'd like it if you got what's
    going on here. That'd be real nice. But it would be a big mistake to think
    we're holding our breath.  We have better things to do than worry about
    whether you'll change in time to get our business. Business is only a part
    of our lives. It seems to be all of yours. Think about it: who needs whom?
    We have real power and we know it. If you don't quite see the light, some
    other outfit will come along that's more attentive, more interesting, more
    fun to play with.  Even at its worst, our newfound conversation is more
    interesting than most trade shows, more entertaining than any TV sitcom, and
    certainly more true-to-life than the corporate web sites we've been seeing.
    Our allegiance is to ourselves—our friends, our new allies and
    acquaintances, even our sparring partners. Companies that have no part in
    this world, also have no future.  Companies are spending billions of dollars
    on Y2K. Why can't they hear this market timebomb ticking? The stakes are
    even higher.  We're both inside companies and outside them. The boundaries
    that separate our conversations look like the Berlin Wall today, but they're
    really just an annoyance. We know they're coming down. We're going to work
    from both sides to take them down.  To traditional corporations, networked
    conversations may appear confused, may sound confusing. But we are
    organizing faster than they are. We have better tools, more new ideas, no
    rules to slow us down.  We are waking up and linking to each other. We are
    watching. But we are not waiting.
    """
  end
end

IO.puts Cluetrain.run
