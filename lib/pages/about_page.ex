defmodule Micaelnussbaumer.AboutPage do
  use Tableau.Page,
    layout: Micaelnussbaumer.RootLayout,
    permalink: "/pages/about",
    sidebar:
      {:safe,
       ["<img src='/images/about/about-2.webp' />", "<img src='/images/about/about-1.webp' />"]}

  use Phoenix.Component

  def greet(assigns) do
    ~H"""
    <img src="/images/about/about-2.webp" />
    <img src="/images/about/about-1.webp" />
    """
  end

  def template(assigns) do
    ~H"""
    <p>
      I work mostly as a web-developer, although I graduated in Photography. I like to own the full-stack development of products, in the following order of preference: back-end systems/engines/schemas, front-end/UI/UX, DevOPs.
      I'm Portuguese but currently based in Thailand.
    </p>
    <br />
    <p>
      I have experience (10++ years) with several technlogies and I prefer to write Elixir as a backend language with whatever combination of ancillary languages/software, when possible. For frontend matters I tend to use WebComponents (the native browser implementation) and plain modern Javascript with Web APIs but I have used extensively in the past frameworks such as Vue2 and React, so I am familiar with them. You cand find some libraries in my
      <a href="https://github.com/mnussbaumer" target="_blank">github</a>
    </p>
    <br />
    <p>
      My preference for Elixir is not an ultimate requirement but sincerely I think it's probably the most well poised language for modern async platforms plus it fits really well into the idea of agentic coding, through its OTP affordances, supervision trees, asynchronous VM, share-nothing contexts - that match really well with the kind of harnesses being built for Agents, while at the same time, due to mostly the same reasons, matching really well to the asynchronous nature of modern websites, platforms and APIs.
    </p>
    <br />
    <p>
      For personal projects I usually orchestrate docker containers with compose, running all the needed stack from databases, telemetry, and so on. This is usually simple but enough to handle whatever it needs initially and enough of a structured base to move into something else if deemed necessary at some point.
    </p>
    <br />
    <p>
      Testing, CI/CD, versioning, documentation, are all essential unless for very specific reasons.
    </p>
    <br />
    <p>TECHNOLOGIES:</p>
    <br />
    <ol class="decorationless-list">
      <li>• Elixir - <span class="since">since 2018</span></li>
      <li>• Ruby on Rails - <span class="since">2016 to 2018</span></li>
      <li>• Javascript - <span class="since">since 2015</span></li>
      <li>• Vue/React - <span class="since">since 2018</span></li>
      <li>• WebComponents - <span class="since">since 2021</span></li>
      <li>• AWS/Digital Ocean - <span class="since">since 2018</span></li>
      <li>• PostgreSQL - <span class="since">since 2016</span></li>
      <li>• Docker/Compose/Swarm - <span class="since">since 2025</span></li>
      <li>• Terraform - <span class="since">since 2025</span></li>
      <li>• K8s - <span class="since">not an expert but have used in previous positions</span></li>
      <li>
        • Several other ancillary technologies -
        <span class="since">bash, linux utils, vm's, html, css, etc...</span>
      </li>
    </ol>
    <br />
    <hr class="hr-separator" />
    <br />
    <p class="accent-highlight-color">Curriculum Vitae</p>
    <br />
    <hr class="hr-separator-b" />
    <ol class="decorationless-list">
      <li><span class="since-b">2026</span></li>
      <li>
        - Resumed work on a browser based TCG, nearing completion for MVP/Alpha, Aether Summon (prev. Aether Wars);
      </li>
      <li>
        - Started moving anti-rota.com to this domain and blog because besides being expensive, in the end squarespace doesn't support a bunch of google adsense/domains things (...);
      </li>
      <li>- Started experimenting with AI for recursive world creation.</li>
      <br />
      <hr class="hr-separator-b" />
      <li>
        <span class="since-b">2024</span>
        - Founded anti-rota.com Blog and Youtube channels dedicated to traveling, digital nomad lifestyle and web-development
      </li>
      <li>• Website www.anti-rota.com (English and Portuguese versions)</li>
      <li>
        • Youtube Channel PT
        <a href="https://www.youtube.com/@anti-rota" target="_blank">youtube.com/@anti-rota</a>
      </li>
      <li>
        • Youtube Channel EN
        <a href="https://www.youtube.com/@anti-route" target="_blank">youtube.com/@anti-route</a>
      </li>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">May 2024 to July 2024</span></li>
      <li>Vacations & OpenSource Work</li>
      <li>
        <a href="https://github.com/mnussbaumer/cssex" target="_blank">
          https://github.com/mnussbaumer/cssex
        </a>
        and personal projects
      </li>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">From January 2022 until 2024</span></li>
      <li>Registering Micael Nussbaumer, UNIPESSOAL LDA (sole-proprietorship)</li>
      <li>
        Lead Dev for QuestOrganizer, INC
        <a href="https://airwander.com" target="_blank">airwander.com</a>
      </li>
      <li>
        Project through <a href="https://www.upwork.com" target="_blank">upwork.com</a>
        for the development of Airwander's (<a href="https://www.airwander.com" target="_blank">airwander.com</a>) flight search website.
      </li>
      <br />
      <li>
        In this project I held several relevant responsibilities in the development of the website from scratch. This included translating figma mockups to a complete functional front-end and back-end, including:
      </li>
      <ul class="decorationless-list">
        <li>• user registration</li>
        <li>
          • recurring subscriptions plans with Stripe, custom check-outs and a custom made hands-off coupon system
        </li>
        <li>• discount codes system</li>
        <li>• email templating</li>
        <li>• Webcomponent system (Javascript)</li>
        <li>• Real-time backend system (Elixir)</li>
        <li>
          • Highly interactive and resilient architecture to enable concurrent interactions with multiple clients and 3rd party providers in real-time
        </li>
        <li>• Styling system (CSSex)</li>
        <li>• Database design for high capacity loads</li>
      </ul>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">October 2021 to July 2023</span></li>
      <li>Flagsmith <a href="https://flagsmith.com/" target="_blank">flagsmith.com</a></li>
      <li>
        Development of the Elixir SDK for the Flagsmith platform:
        <a href="https://github.com/Flagsmith/" target="_blank">github.com/Flagsmith</a>
      </li>
      <br />
      <ul>
        <li>flagsmith-elixir-client</li>
        <li>• Translating a python SDK library into Elixir in an idiomatic and performant way</li>
        <li>• Documenting and typespec'ing the library</li>
        <li>
          • Writing a robust test-suite to guarantee complete adherence to the client expectations and
        </li>
        <li>ensuring the library behaves correctly at all times</li>
        <li>
          • Suggesting a way to keep parity of tests between all SDKs and implementing the Elixir part of it
        </li>
      </ul>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">April 2021 to December 2021</span></li>
      <li><a href="https://www.scientiamobile.com/" target="_blank">scientiamobile.com</a></li>
      <li>
        Worked on writing articles about mobile optimisation and its importance for non-developers and developed several libraries for client detection
        <a href="https://github.com/imgeng/imageengine-cms-detector" target="_blank">
          imageengine-cms-detector
        </a>
      </li>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">April 2021 to August 2021</span></li>
      <li>DATAIKU / BigDropInc</li>
      <li>
        Development of the Linear Regression Game
        <a href="https://regression.historyofdatascience.com/" target="_blank">
          regression.historyofdatascience.com
        </a>
        featured in
        <a href="https://www.historyofdatascience.com/" target="_blank">historyofdatascience.com</a>
        working for BigDropInc, in turn working for DATAIKU (<a
          href="https://www.bigdropinc.com/work/dataiku-enterprise-ai/"
          target="_blank"
        >bigdropinc.com</a>)
      </li>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">August 2020 to April 2021</span></li>
      <br />
      <li>Vacations/Sabbatical</li>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">December 2017 to August 2020</span></li>
      <li>smallworld</li>
      <li>
        Working as a contractor for
        <a href="https://www.smallworldus.com/" target="_blank">www.smallworldus.com</a>
        , development of several applications and websites initially in Ruby on Rails and later moving to Elixir
      </li>
      <ul>
        <li>• Front-end development according to mockups, vanilla JS, VueJS, React</li>
        <li>• Back-end development for diﬀerent systems</li>
      </ul>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">2016</span></li>
      <br />
      <li>Several Web Development Small Scope Projects</li>
      <li>
        Several small duration contracts/gigs as I gained experience as a web-developer, working mostly in Ruby on Rails & Javascript, HTML & CSS
      </li>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">2014 to 2016</span></li>
      <br />
      <li>Working remotely as professional Image Editor for diﬀerent clients around the world.</li>
      <li>
        Traveled the world for about 2 years visiting over 12 countries and stayed for a while in each while working remotely. Started coding again. I had a
        <a href="https://micaelnussbaumer.wordpress.com">old wordpress</a>
        that I started at the time and later wrote content and photography for a shared blog (staygypsy.com) (these are very old blogs and they might not reflect any current view of mine, but since they are there I just leave it be as they're interesting fragments, to me personally).
      </li>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">2013 to 2014</span></li>
      <br />
      <li>Grant in Ninho de Empresas</li>
      <li>
        Applied and was awarded a 1 year grant to develop photography work in the entrepreneurship hub in Ferreira do Alentejo, Portugal
      </li>
      <ul>
        <li>
          • Photography work for BellolivaOil photographing their products, team, factory and production fields near Beja
        </li>
        <li>
          • Photography work for Partido Socialista political party, campaign portraits for brochures and outdoors (Ferreira do Alentejo)
        </li>
        <li>• Video work for <a href="https://thermotelha.pt/" target="_blank">thermotelha.pt</a></li>
        <li>• Several photography assignments for local clients and small organisations</li>
      </ul>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">2013</span></li>
      <li>
        Participation in the collective exhibition "Buildings & Remnants", by Fundação Cidade de Guimarães during Capital Europeia da Cultura (Guimarães European Capital of Culture) (<a
          href="https://cargocollective.com/micaelnussbaumer/Tempo-imprime-no-espaco"
          target="_blank"
        >check in cargocollective.com</a>)
      </li>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">2012 to 2013</span></li>
      <li>Work as independent freelancer photographer</li>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">February 2012 to May 2012</span></li>
      <br />
      <li>Leonardo DaVinci Grant internship</li>
      <li>
        Selected for through checkin.org.pt organisation for a Leonardo DaVinci grant, a 3 month stay in Padova, Italy doing an internship with
        <a href="https://www.lucamasara.it/" target="_blank">lucamasara.it</a>
      </li>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">2011 to 2012</span></li>
      <li>Internship in «Fátima Missionária» (Missionários da Consolata PT)</li>
      <li>
        Describing and Cataloguing the photographic collection of the Institute,
        <a href="https://www.consolata.pt/centro-missionario-allamano-fatima/" target="_blank">
          Centro Missionário Allamano, Fátima
        </a>
      </li>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">2007 to 2010</span></li>
      <br />
      <li>Degree in Photography</li>
      <li>
        Finished the 3 year licentiate degree in Photography through the public university/polytechnic Instituto Politécnico de Tomar.
      </li>
      <li>
        - Founding member of the Photography Student's Association from the first year to the last
      </li>
      <li>
        - Organized weekly free cinema sessions & discussions, held on the Institute's amphitheater, with a program curated for the photography student's curriculum
      </li>
      <li>- Regular participation in activity organisation for the Association</li>
      <li>
        - <span class="since">16 June 2010 to 4 July 2010</span>
        Personal Exhibition "O Registador" (<a
          href="https://cargocollective.com/micaelnussbaumer/O-Registador"
          target="_blank"
        >in cargocollective.com</a>)
      </li>
      <br />
      <hr class="hr-separator-b" />
      <li><span class="since-b">Before 2008</span></li>
      <li>
        • Almost 4 Years working as a waiter in restaurants and weddings, Badoca Safari Park (almost 2 years, regular room waiter and special services;) and Baguinho Café (1 year, regular and weddings while finishing Highschool at night, 1 year weddings while on the first year of my Licenciature)
      </li>
      <li>• Professional licence for Polyethylene Wielding followed by 3 month internship</li>
    </ol>
    <br />
    """
  end
end
