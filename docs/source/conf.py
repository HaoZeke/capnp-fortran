project = "capnp-fortran"
copyright = "2026, Rohit Goswami"
author = "Rohit Goswami"
release = "0.1.0"

extensions = [
    "sphinx.ext.graphviz",
    "sphinxcontrib.bibtex",
    "sphinx_copybutton",
    "sphinx_design",
    "myst_parser",
    "sphinxcontrib.mermaid",
]

bibtex_bibfiles = ["references.bib"]
bibtex_default_style = "alpha"
bibtex_reference_style = "author_year"

templates_path = ["_templates"]
exclude_patterns = ["_build"]

myst_enable_extensions = [
    "colon_fence",
    "deflist",
]
myst_fence_as_directive = ["mermaid"]

html_theme = "shibuya"
html_static_path = ["_static"]
html_title = "capnp-fortran documentation"
html_baseurl = "https://haozeke.github.io/capnp-fortran/"

# Edit-this-page + repo-stats sidebars (Shibuya built-ins)
html_context = {
    "source_type": "github",
    "source_user": "HaoZeke",
    "source_repo": "capnp-fortran",
    "source_version": "main",
    "source_docs_path": "/docs/source/",
}

html_sidebars = {
    "**": [
        "sidebars/localtoc.html",
        "sidebars/repo-stats.html",
        "sidebars/edit-this-page.html",
    ],
}

html_theme_options = {
    "github_url": "https://github.com/HaoZeke/capnp-fortran",
    "accent_color": "indigo",
    "dark_code": True,
    "globaltoc_expand_depth": 1,
    "nav_links": [
        {
            "title": "Tutorial",
            "url": "tutorial",
            "summary": "Write/read a message; typed RPC client and server",
        },
        {
            "title": "Architecture",
            "url": "architecture",
            "summary": "Wire format, arena, two-pass emitter, vat state machine",
        },
        {
            "title": "Interop",
            "url": "interop",
            "summary": "c-capnproto golden master, capnp-C++ RPC peer",
        },
        {"title": "RPC", "url": "rpc", "summary": "Two-party RPC tier"},
        {"title": "Reference", "url": "reference", "summary": "Full public API"},
        {"title": "GitHub", "url": "https://github.com/HaoZeke/capnp-fortran"},
    ],
}

copybutton_prompt_text = r">>> |\.\.\. |\$ |In \[\d*\]: | {2,5}\.\.\.: | {5,8}: "
copybutton_prompt_is_regexp = True
# Do not copy line numbers / prompts; keep blocks paste-friendly
copybutton_exclude = ".linenos, .gp, .go"
copybutton_line_continuation_character = "\\"
copybutton_here_doc_delimiter = "EOF"
