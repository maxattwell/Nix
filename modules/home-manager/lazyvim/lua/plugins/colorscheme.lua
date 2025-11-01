return {
  {
    "ellisonleao/gruvbox.nvim",
    priority = 1000,
    config = function()
      require("gruvbox").setup({
        transparent_mode = false,
        contrast = "hard", -- can be "hard", "soft" or empty string
      })
      -- Set to light mode
      vim.o.background = "light"
    end,
  },
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "gruvbox",
    },
  },
}
