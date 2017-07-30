module Themes where

import Data.Monoid ((<>))

import Yi hiding (option)
import Yi.Config.Simple.Types
import Yi.Style hiding (cyan, red, magenta, green, blue)

fg = withFg
bg = withBg

italicized = withItlc True
reversed = withReverse True
emboldened = withBd True
underlined = withUnderline True

solarizedDark :: Theme
solarizedDark = Proto $ const UIStyle
  { modelineAttributes = emptyAttributes { foreground = base03, background = base00 }
  , modelineFocusStyle = fg base1 <> bg base1

  , tabBarAttributes   = emptyAttributes { foreground = base03, background = base1 }
  , tabInFocusStyle    = fg base1 <> bg base03
  , tabNotFocusedStyle = fg base1 <> bg base03

  , baseAttributes     = emptyAttributes { foreground = base1, background = base03 }

  , selectedStyle      = fg base01 <> bg base2
  , eofStyle           = fg red
  , errorStyle         = reversed <> bg red
  , hintStyle          = bg base03
  , strongHintStyle    = bg blue

  , commentStyle       = fg base0
  , blockCommentStyle  = fg base1
  , keywordStyle       = fg green
  , variableStyle      = fg cyan
  , operatorStyle      = fg red
  , numberStyle        = fg orange
  , preprocessorStyle  = fg magenta
  , stringStyle        = fg violet
  , longStringStyle    = fg violet

  , typeStyle          = fg blue
  , dataConstructorStyle
                       = italicized <> fg blue
  , builtinStyle       = fg yellow

  , importStyle        = fg blue
  , regexStyle         = fg red

  , makeFileRuleHead   = fg blue
  , makeFileAction     = fg grey
  , quoteStyle         = fg grey
  }
  where
    base03  = RGB   0  43  54
    base02  = RGB   7  54  66
    base01  = RGB  88 110 117
    base00  = RGB 101 123 131
    base0   = RGB 131 148 150
    base1   = RGB 147 161 161
    base2   = RGB 238 232 213
    base3   = RGB 253 246 227
    yellow  = RGB 181 137   0
    orange  = RGB 203  75  22
    red     = RGB 220  50  47
    magenta = RGB 211  54 130
    violet  = RGB 108 113 196
    blue    = RGB  38 139 210
    cyan    = RGB  42 161 152
    green   = RGB 133 153   0 

spacemacsDark :: Theme
spacemacsDark = Proto $ const UIStyle
  { modelineAttributes = emptyAttributes { foreground = base03, background = base00 }
  , modelineFocusStyle = fg base1 <> bg base1

  , tabBarAttributes   = emptyAttributes { foreground = base03, background = base1 }
  , tabInFocusStyle    = fg base1 <> bg base03
  , tabNotFocusedStyle = fg base1 <> bg base03

  , baseAttributes     = emptyAttributes { foreground = base1, background = base03 }

  , selectedStyle      = fg base01 <> bg base2
  , eofStyle           = fg red
  , errorStyle         = bg red
  , hintStyle          = bg base03
  , strongHintStyle    = bg blue

  , commentStyle       = fg base0
  , blockCommentStyle  = fg base0
  , keywordStyle       = fg green
  , variableStyle      = fg cyan
  , operatorStyle      = fg red
  , numberStyle        = fg lnum
  , preprocessorStyle  = fg magenta

  , stringStyle        = fg str
  , longStringStyle    = fg str

  , typeStyle          = fg blue
  , dataConstructorStyle
                       = fg blue
  , builtinStyle       = fg yellow

  , importStyle        = fg green
  , regexStyle         = fg red

  , makeFileRuleHead   = fg blue
  , makeFileAction     = fg grey
  , quoteStyle         = fg grey
  }

  where
    base03 = bg1
    base00 = bg1
    base1 = bg1
    base2 = bg1
    base01 = bg1
    base0 = bg1
    gray1 = RGB 0x21 0x20 0x26
    gray2 = RGB 0x29 0x2b 0x2e
    gray3 = RGB 0x44 0x41 0x55
    gray4 = RGB 0x44 0x50 0x5c
    gray5 = RGB 0xb2 0xb2 0xb2
    
    yellow = RGB 0xb1 0x95 0x1d
    
    pink1 = RGB 0xa4 0x5b 0xad
    pink2 = RGB 0xbc 0x6e 0xc5
    teal = RGB 0x2a 0xa1 0xae
    darkTeal = RGB 0x29 0x2e 0x34
    blue = RGB 0x4f 0x97 0xd7
    green = RGB 0x2d 0x95 0x74
    red = RGB 0xce 0x53 0x7a
    lightBlue = RGB 0x75 0x90 0xdb
    
    bg1 = RGB 0x29 0x2b 0x2e
    bg2 = RGB 0x21 0x20 0x26
    base = RGB 0xb2 0xb2 0xb2
    cursor = RGB 0xb1 0x95 0x1d
    const_ = RGB 0xa4 0x5b 0xad
    comment = RGB 0x2a 0xa1 0xae
    commentBg = RGB 0x29 0x2e 0x34
    func = RGB 0xbc 0x6e 0xc5
    highlight = RGB 0x44 0x41 0x55
    keyword = RGB 0x4f 0x97 0xd7
    str = RGB 0x2d 0x95 0x74
    type_ = RGB 0xce 0x53 0x7a
    var = RGB 0x75 0x90 0xdb
    lnum = RGB 0x44 0x50 0x5c

