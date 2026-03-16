import Config

config :tableau, Tableau.PageExtension, dir: ["_pages", "_wip"]

config :tableau, Tableau.PostExtension,
  dir: ["_posts", "_drafts"],
  enabled: true,
  future: false,
  permalink: "/posts/:slug",
  author: "micael nussbaumer",
  layout: Micaelnussbaumer.PostLayout
