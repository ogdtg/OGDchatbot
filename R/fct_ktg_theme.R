#' Create the Kanton Thurgau Bootstrap Theme
#'
#' Generates a Bootstrap 5 theme customized with Kanton Thurgau colors, typography, and layout tokens.
#'
#' @return A `bslib::bs_theme` object with custom CSS rules applied
#' @export
#' @importFrom bslib bs_theme bs_add_rules
create_ktg_theme <- function() {
  theme <- bs_theme(
    version = 5,
    primary = "#327A1E",
    font_scale = 1
  )

  theme <- bs_add_rules(theme, "
    .pagination-controls {
      display: flex;
      flex-direction: column;
      align-items: center;
      gap: 1.5rem 0;
    }

    @media (min-width: 1056px) {
      .pagination-controls {
        flex-direction: row;
        align-items: baseline;
        gap: 0 1rem;
      }
    }

    .ktg-title-01 {
      font-size: calc(1rem * 2.625 + 2.125rem);
      font-weight: 500;
    }
  ")

  theme <- bs_add_rules(theme, "
    body, html {
      margin: 0;
      padding: 0;
    }

    .ktg-top-stripes {
      display: grid;
      grid-template-rows: 0.5rem 0.5rem 0.5rem 0.5rem 0.5rem;
      width: 100vw;
      margin: 0;
      padding: 0;
      position: relative;
      z-index: 1000;
    }

    .stripe {
      width: 100%;
      height: 100%;
    }

    .stripe-1 { background-color: #327A1E; }
    .stripe-2 { background-color: #DAEDFF; }
    .stripe-3 { background-color: #A1CEF7; }
    .stripe-4 { background-color: #FDE303; }
    .stripe-5 { background-color: #F24822; }
  ")

  theme <- bs_add_rules(theme, "
    @font-face {
      font-family: 'AktivGrotesk';
      src: url('https://schalter.tg.ch/.resources/foundation/webresources/fonts/AktivGrotesk_W_Md.woff') format('woff');
      font-weight: 500;
      font-style: normal;
    }

    body, .h1, .h2, .h3, .h4, .h5, .h6 {
      font-family: 'AktivGrotesk', sans-serif !important;
    }
  ")

  theme <- bs_add_rules(theme, "
    .ktg-top-stripes {
      display: grid;
      grid-template-rows: 0.5rem 0.5rem 0.5rem 0.5rem 1rem;
      width: 100%;
    }

    .stripe { width: 100%; height: 100%; }
    .stripe-1 { background-color: #FFED00; }
    .stripe-2 { background-color: #ADC939; }
    .stripe-3 { background-color: #74A732; }
    .stripe-4 { background-color: #00A4E8; }
    .stripe-5 { background-color: #4DB5EB; }
  ")

  theme <- bs_add_rules(theme, "
    .btn-tg {
      background-color: #327A1E;
      color: white;
      border: none;
    }

    .btn-tg:hover,
    .btn-tg:focus,
    .btn-tg:active {
      background-color: #176822;
      color: white;
    }
  ")

  theme <- bs_add_rules(theme, "
    .bslib-sidebar-layout .bslib-sidebar {
      background-color: #F5F5F5 !important;
    }

    .bslib-sidebar-layout .bslib-main {
      background-color: #FFFFFF !important;
    }
  ")

  return(theme)
}
