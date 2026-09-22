<!-- shiny-app-builder:start -->
## Explore It with Shiny

[Connect the app directly to the page's central teaching claim.]

<div class="shiny-preview" aria-labelledby="shiny-preview-title">
  <div class="shiny-preview-label" id="shiny-preview-title">Static preview</div>
  <p class="shiny-preview-note">This preview shows one app state and is not interactive.</p>
  <div class="shiny-preview-layout">
    <aside class="shiny-preview-controls" aria-label="Example app controls">
      [Use disabled native controls or non-control representations.]
    </aside>
    <div class="shiny-preview-output">
      [Show generated default-state values, an accessible plot, and a compact interpretation.]
    </div>
  </div>
</div>

[Download the complete app](downloads/{{APP_SLUG}}-shiny.zip){download="{{APP_SLUG}}-shiny.zip"}

After extracting the ZIP, install `shiny` if needed, open R in the extracted app folder, and run:

```{r}
#| eval: false
shiny::runApp()
```

[State the source example's statistical and data limitations.]
<!-- shiny-app-builder:end -->
