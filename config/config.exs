import Config

config :tableau, :reloader,
  patterns: [
    ~r"^lib/.*.ex",
    ~r"^(_posts|_pages)/.*.md",
    ~r"^assets/.*.(css|js)"
  ]

config :web_dev_utils, :reload_log, true
# uncomment this if you use something like ngrok
# config :web_dev_utils, :reload_url, "'wss://' + location.host + '/ws'"

config :esbuild,
  version: "0.25.5",
  default: [
    args: ~w(js/site.js --bundle --target=es2016 --outdir=../_site/js),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

config :tableau, :assets, esbuild: {Esbuild, :install_and_run, [:default, ~w(--watch)]}

config :tableau, :config,
  url: "http://localhost:4999",
  markdown: [
    mdex: [
      extension: [
        table: true,
        header_ids: "",
        tasklist: true,
        strikethrough: true,
        autolink: true,
        alerts: true,
        footnotes: true
      ],
      render: [unsafe: true],
      syntax_highlight: [
        formatter: {
          :html_inline,
          theme: "nightfox"
        }
      ]
    ]
  ]

config :tableau, Tableau.PageExtension, enabled: true
config :tableau, Tableau.DataExtension, enabled: true
config :tableau, Tableau.SitemapExtension, enabled: true

config :tableau, Tableau.PostExtension,
  enabled: true,
  future: false,
  permalink: "/posts/:slug",
  author: "micael nussbaumer",
  layout: Micaelnussbaumer.PostLayout

config :tableau, Tableau.RSSExtension,
  enabled: true,
  title: "micaelnussbaumer",
  description: "Personal Blog & Site"

config :elixir, :time_zone_database, Tz.TimeZoneDatabase

import_config "#{Mix.env()}.exs"
