defmodule Couchbeam.Mixfile do
use Mix.Project

    def project do
        [
            app: :couchbeam,
            version: "1.4.2",
            description: "Erlang CouchDB client",
            deps: deps,
            package: package,
            language: :erlang
        ]
    end

    def application do
        [
            applications:
                [
                    :kernel,
                    :stdlib,
                    :crypto,
                    :asn1,
                    :public_key,
                    :ssl,
                    :hackney
                ],
            env:
                [
                ],
            mod: {:couchbeam_app, []}
        ]
    end

    def deps do
        [
            {:hackney, "~> 1.6.2"},
            {:jsx, "~> 2.8.0"}
        ]
    end

    defp package do
        [
            files: [
                "src",
                "include",
                "mix.exs",
                "mix.lock",
                "rebar.config",
                "rebar.lock",
                "README.md",
                "NEWS.md",
                "LICENSE",
                "NOTICE"
            ],
            maintainers: ["Benoit Chesneau"],
            licenses: ["MIT"],
            links: %{"Github" => "https://github.com/benoitc/couchbeam"}
        ]
    end
end
