object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'MailRelay for Oracle APEX'
  ClientHeight = 900
  ClientWidth = 1420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnDestroy = FormDestroy
  TextHeight = 15
  object PageMain: TPageControl
    Left = 0
    Top = 0
    Width = 1420
    Height = 881
    ActivePage = TabDashboard
    Align = alClient
    TabOrder = 0
    object TabDashboard: TTabSheet
      Caption = 'Dashboard'
      object PnlDashTop: TPanel
        Left = 0
        Top = 0
        Width = 1412
        Height = 192
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object SplDashQueue: TSplitter
          Left = 330
          Top = 0
          Width = 6
          Height = 192
          Enabled = False
          MinSize = 220
          Visible = False
        end
        object SplDashActions: TSplitter
          Left = 576
          Top = 0
          Width = 6
          Height = 192
          Enabled = False
          MinSize = 180
          Visible = False
        end
        object GbDashQueue: TGroupBox
          Left = 0
          Top = 0
          Width = 330
          Height = 192
          Align = alLeft
          Caption = 'Queue'
          TabOrder = 0
          object LblQueueSizeCaption: TLabel
            Left = 16
            Top = 32
            Width = 60
            Height = 15
            Caption = 'Queue size:'
          end
          object LblQueueSizeValue: TLabel
            Left = 188
            Top = 32
            Width = 6
            Height = 15
            Caption = '0'
          end
          object LblInflightCaption: TLabel
            Left = 16
            Top = 58
            Width = 46
            Height = 15
            Caption = 'In-flight:'
          end
          object LblInflightValue: TLabel
            Left = 188
            Top = 58
            Width = 6
            Height = 15
            Caption = '0'
          end
          object LblDeferredCaption: TLabel
            Left = 16
            Top = 84
            Width = 48
            Height = 15
            Caption = 'Deferred:'
          end
          object LblDeferredValue: TLabel
            Left = 188
            Top = 84
            Width = 6
            Height = 15
            Caption = '0'
          end
          object LblDeadCaption: TLabel
            Left = 16
            Top = 110
            Width = 30
            Height = 15
            Caption = 'Dead:'
          end
          object LblDeadValue: TLabel
            Left = 188
            Top = 110
            Width = 6
            Height = 15
            Caption = '0'
          end
          object LblOldestCaption: TLabel
            Left = 16
            Top = 136
            Width = 65
            Height = 15
            Caption = 'Oldest (sec):'
          end
          object LblOldestValue: TLabel
            Left = 188
            Top = 136
            Width = 6
            Height = 15
            Caption = '0'
          end
        end
        object GbDashActions: TGroupBox
          Left = 336
          Top = 0
          Width = 240
          Height = 192
          Align = alLeft
          Caption = 'Actions'
          TabOrder = 1
          object LblDashStatePersistNote: TLabel
            Left = 16
            Top = 163
            Width = 205
            Height = 25
            AutoSize = False
            Caption = 'Server will save it'#39's state after restart.'
            WordWrap = True
          end
          object BtnPurgeDead: TButton
            Left = 16
            Top = 30
            Width = 205
            Height = 27
            Caption = 'Purge dead-letters'
            TabOrder = 0
            OnClick = OnPurgeDead
          end
          object BtnFlush: TButton
            Left = 16
            Top = 63
            Width = 205
            Height = 27
            Caption = 'Flush now'
            TabOrder = 1
            OnClick = OnFlush
          end
          object BtnStartListener: TButton
            Left = 16
            Top = 96
            Width = 205
            Height = 27
            Caption = 'Start listener'
            TabOrder = 2
            OnClick = OnStartListener
          end
          object BtnStopListener: TButton
            Left = 16
            Top = 96
            Width = 205
            Height = 27
            Caption = 'Stop listener'
            TabOrder = 3
            Visible = False
            OnClick = OnStopListener
          end
          object BtnStartSenders: TButton
            Left = 16
            Top = 129
            Width = 205
            Height = 27
            Caption = 'Start senders'
            TabOrder = 4
            OnClick = OnStartSenders
          end
          object BtnStopSenders: TButton
            Left = 16
            Top = 129
            Width = 205
            Height = 27
            Caption = 'Stop senders'
            TabOrder = 5
            Visible = False
            OnClick = OnStopSenders
          end
        end
        object GbDashRelayStatus: TGroupBox
          Left = 582
          Top = 0
          Width = 830
          Height = 192
          Align = alClient
          Caption = 'Relay status'
          TabOrder = 2
          object LblListenerStatusCaption: TLabel
            Left = 16
            Top = 32
            Width = 76
            Height = 15
            Caption = 'Status listener:'
          end
          object LblListenerStatusValue: TLabel
            Left = 206
            Top = 32
            Width = 48
            Height = 15
            Caption = 'STOPPED'
          end
          object LblSendersStatusCaption: TLabel
            Left = 16
            Top = 58
            Width = 78
            Height = 15
            Caption = 'Status senders:'
          end
          object LblSendersStatusValue: TLabel
            Left = 206
            Top = 58
            Width = 48
            Height = 15
            Caption = 'STOPPED'
          end
          object LblListenerSessionsCaption: TLabel
            Left = 16
            Top = 84
            Width = 123
            Height = 15
            Caption = 'Active listener sessions:'
          end
          object LblListenerSessionsValue: TLabel
            Left = 206
            Top = 84
            Width = 6
            Height = 15
            Caption = '0'
          end
          object LblActiveSendersCaption: TLabel
            Left = 16
            Top = 110
            Width = 79
            Height = 15
            Caption = 'Active senders:'
          end
          object LblActiveSendersValue: TLabel
            Left = 206
            Top = 110
            Width = 6
            Height = 15
            Caption = '0'
          end
        end
      end
      object GbDashHourly: TGroupBox
        Left = 0
        Top = 192
        Width = 1412
        Height = 280
        Align = alTop
        Caption = 'Messages per hour (last 24h)'
        TabOrder = 1
        object ChartHourly: TChart
          Left = 2
          Top = 17
          Width = 1408
          Height = 261
          AllowPanning = pmNone
          Title.Text.Strings = (
            'Hourly volume')
          BottomAxis.Grid.Visible = False
          LeftAxis.Grid.Visible = False
          View3D = False
          Align = alClient
          TabOrder = 0
          DefaultCanvas = 'TGDIPlusCanvas'
          ColorPaletteIndex = 13
        end
      end
      object PnlDashPies: TPanel
        Left = 0
        Top = 472
        Width = 1412
        Height = 379
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
        object SplDashInbound: TSplitter
          Left = 430
          Top = 0
          Width = 6
          Height = 379
          Enabled = False
          MinSize = 260
          Visible = False
          ExplicitHeight = 398
        end
        object SplDashOutbound: TSplitter
          Left = 866
          Top = 0
          Width = 6
          Height = 379
          Enabled = False
          MinSize = 260
          Visible = False
          ExplicitHeight = 398
        end
        object GbDashInboundChart: TGroupBox
          Left = 0
          Top = 0
          Width = 430
          Height = 379
          Align = alLeft
          Caption = 'Mail distribution inbound'
          TabOrder = 0
          object ChartInbound: TChart
            Left = 2
            Top = 17
            Width = 426
            Height = 360
            Title.Text.Strings = (
              'Inbound distribution')
            View3D = False
            Align = alClient
            TabOrder = 0
            DefaultCanvas = 'TGDIPlusCanvas'
            ColorPaletteIndex = 13
          end
        end
        object GbDashOutboundChart: TGroupBox
          Left = 436
          Top = 0
          Width = 430
          Height = 379
          Align = alLeft
          Caption = 'Mail distribution outbound'
          TabOrder = 1
          object ChartOutbound: TChart
            Left = 2
            Top = 17
            Width = 426
            Height = 360
            Title.Text.Strings = (
              'Outbound distribution')
            View3D = False
            Align = alClient
            TabOrder = 0
            DefaultCanvas = 'TGDIPlusCanvas'
            ColorPaletteIndex = 13
          end
        end
        object GbDashProblemsChart: TGroupBox
          Left = 872
          Top = 0
          Width = 540
          Height = 379
          Align = alClient
          Caption = 'Problematic mail percentage'
          TabOrder = 2
          object ChartProblems: TChart
            Left = 2
            Top = 17
            Width = 536
            Height = 360
            Title.Text.Strings = (
              'Problematic percentage')
            View3D = False
            Align = alClient
            TabOrder = 0
            DefaultCanvas = 'TGDIPlusCanvas'
            ColorPaletteIndex = 13
          end
        end
      end
    end
    object TabConfig: TTabSheet
      Caption = 'Config'
      ImageIndex = 1
      object ScrollConfig: TScrollBox
        Left = 0
        Top = 0
        Width = 1412
        Height = 851
        VertScrollBar.Tracking = True
        Align = alClient
        BorderStyle = bsNone
        TabOrder = 0
        ExplicitHeight = 870
        object PnlConfigTop: TPanel
          Left = 0
          Top = 0
          Width = 1412
          Height = 500
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object SplConfigLeft: TSplitter
            Left = 560
            Top = 0
            Width = 6
            Height = 500
            Enabled = False
            MinSize = 360
            Visible = False
          end
          object PnlConfigLeft: TPanel
            Left = 0
            Top = 0
            Width = 560
            Height = 500
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 0
            object SplConfigInbound: TSplitter
              Left = 0
              Top = 250
              Width = 560
              Height = 6
              Cursor = crVSplit
              Align = alTop
              Enabled = False
              MinSize = 170
              Visible = False
            end
            object GbInbound: TGroupBox
              Left = 0
              Top = 0
              Width = 560
              Height = 250
              Align = alTop
              Caption = 'Inbound config'
              TabOrder = 0
              object PnlInboundBody: TPanel
                Left = 2
                Top = 17
                Width = 556
                Height = 231
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                object PnlInboundRowBindIP: TPanel
                  Left = 0
                  Top = 0
                  Width = 556
                  Height = 34
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 0
                  object LblBindIP: TLabel
                    Left = 180
                    Top = 0
                    Width = 40
                    Height = 15
                    Align = alLeft
                    Alignment = taRightJustify
                    Caption = 'Bind IP:'
                    Layout = tlCenter
                  end
                  object EdBindIP: TEdit
                    Left = 230
                    Top = 5
                    Width = 320
                    Height = 23
                    Align = alRight
                    TabOrder = 0
                  end
                end
                object PnlInboundRowBindIPHelp: TPanel
                  Left = 0
                  Top = 34
                  Width = 556
                  Height = 28
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 1
                  object LblBindIPHelp: TLabel
                    Left = 16
                    Top = 6
                    Width = 534
                    Height = 18
                    AutoSize = False
                    Caption = 'Listener binding interface. Example: 127.0.0.1'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clGrayText
                    Font.Height = -11
                    Font.Name = 'Segoe UI'
                    Font.Style = []
                    ParentFont = False
                    WordWrap = True
                  end
                end
                object PnlInboundRowBindPort: TPanel
                  Left = 0
                  Top = 62
                  Width = 556
                  Height = 34
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 2
                  object LblBindPort: TLabel
                    Left = 168
                    Top = 0
                    Width = 52
                    Height = 15
                    Align = alLeft
                    Alignment = taRightJustify
                    Caption = 'Bind port:'
                    Layout = tlCenter
                  end
                  object EdBindPort: TEdit
                    Left = 230
                    Top = 5
                    Width = 320
                    Height = 23
                    Align = alRight
                    TabOrder = 0
                  end
                end
                object PnlInboundRowAllowedIP: TPanel
                  Left = 0
                  Top = 96
                  Width = 556
                  Height = 34
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 3
                  object LblAllowedIP: TLabel
                    Left = 161
                    Top = 0
                    Width = 59
                    Height = 15
                    Align = alLeft
                    Alignment = taRightJustify
                    Caption = 'Allowed IP:'
                    Layout = tlCenter
                  end
                  object EdAllowedIP: TEdit
                    Left = 230
                    Top = 5
                    Width = 320
                    Height = 23
                    Align = alRight
                    TabOrder = 0
                  end
                end
                object PnlInboundRowAllowedIPHelp: TPanel
                  Left = 0
                  Top = 130
                  Width = 556
                  Height = 28
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 4
                  object LblAllowedIPHelp: TLabel
                    Left = 16
                    Top = 6
                    Width = 534
                    Height = 18
                    AutoSize = False
                    Caption = 'Allowed SMTP clients. Example: 127.0.0.1, 212.12.27.17'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clGrayText
                    Font.Height = -11
                    Font.Name = 'Segoe UI'
                    Font.Style = []
                    ParentFont = False
                    WordWrap = True
                  end
                end
                object PnlInboundRowMaxMessage: TPanel
                  Left = 0
                  Top = 158
                  Width = 556
                  Height = 34
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 5
                  object LblMaxMessageMB: TLabel
                    Left = 94
                    Top = 0
                    Width = 126
                    Height = 15
                    Align = alLeft
                    Alignment = taRightJustify
                    Caption = 'Max message size (MB):'
                    Layout = tlCenter
                  end
                  object EdMaxMessageMB: TEdit
                    Left = 230
                    Top = 5
                    Width = 320
                    Height = 23
                    Align = alRight
                    TabOrder = 0
                  end
                end
              end
            end
            object GbInboundAuth: TGroupBox
              Left = 0
              Top = 256
              Width = 560
              Height = 224
              Align = alTop
              Caption = 'Inbound auth config'
              TabOrder = 1
              object PnlInboundAuthBody: TPanel
                Left = 2
                Top = 17
                Width = 556
                Height = 211
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                object PnlInboundAuthRowUser: TPanel
                  Left = 0
                  Top = 0
                  Width = 556
                  Height = 34
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 0
                  object LblInboundAuthUser: TLabel
                    Left = 164
                    Top = 0
                    Width = 56
                    Height = 15
                    Align = alLeft
                    Alignment = taRightJustify
                    Caption = 'Username:'
                    Layout = tlCenter
                  end
                  object EdInboundAuthUser: TEdit
                    Left = 230
                    Top = 5
                    Width = 320
                    Height = 23
                    Align = alRight
                    TabOrder = 0
                  end
                end
                object PnlInboundAuthRowPass: TPanel
                  Left = 0
                  Top = 34
                  Width = 556
                  Height = 34
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 1
                  object LblInboundAuthPassword: TLabel
                    Left = 167
                    Top = 0
                    Width = 53
                    Height = 15
                    Align = alLeft
                    Alignment = taRightJustify
                    Caption = 'Password:'
                    Layout = tlCenter
                  end
                  object EdInboundAuthPassword: TEdit
                    Left = 230
                    Top = 5
                    Width = 320
                    Height = 23
                    Align = alRight
                    PasswordChar = '*'
                    TabOrder = 0
                  end
                end
                object PnlInboundAuthOptions: TPanel
                  Left = 0
                  Top = 68
                  Width = 556
                  Height = 58
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 2
                  object CbRequireInboundAuth: TCheckBox
                    Left = 16
                    Top = 8
                    Width = 520
                    Height = 17
                    Align = alTop
                    Caption = 'Require auth credentials'
                    TabOrder = 0
                  end
                  object BtnResetInboundAuth: TButton
                    Left = 16
                    Top = 31
                    Width = 130
                    Height = 27
                    Caption = 'Reset'
                    TabOrder = 1
                    OnClick = OnResetInboundAuth
                  end
                end
              end
            end
          end
          object PnlConfigRight: TPanel
            Left = 566
            Top = 0
            Width = 846
            Height = 500
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            object SplConfigOutbound: TSplitter
              Left = 0
              Top = 200
              Width = 846
              Height = 6
              Cursor = crVSplit
              Align = alTop
              Enabled = False
              MinSize = 150
              Visible = False
            end
            object GbOutbound: TGroupBox
              Left = 0
              Top = 0
              Width = 846
              Height = 200
              Align = alTop
              Caption = 'Outbound config'
              TabOrder = 0
              ExplicitWidth = 852
              object PnlOutboundBody: TPanel
                Left = 2
                Top = 17
                Width = 842
                Height = 181
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                ExplicitWidth = 848
                object PnlOutboundRowHost: TPanel
                  Left = 0
                  Top = 0
                  Width = 842
                  Height = 34
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 0
                  ExplicitWidth = 848
                  object LblOutHost: TLabel
                    Left = 161
                    Top = 0
                    Width = 59
                    Height = 15
                    Align = alLeft
                    Alignment = taRightJustify
                    Caption = 'SMTP host:'
                    Layout = tlCenter
                  end
                  object EdOutHost: TEdit
                    Left = 230
                    Top = 5
                    Width = 602
                    Height = 23
                    Align = alRight
                    TabOrder = 0
                  end
                end
                object PnlOutboundRowPort: TPanel
                  Left = 0
                  Top = 34
                  Width = 842
                  Height = 34
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 1
                  ExplicitWidth = 848
                  object LblOutPort: TLabel
                    Left = 162
                    Top = 0
                    Width = 58
                    Height = 15
                    Align = alLeft
                    Alignment = taRightJustify
                    Caption = 'SMTP port:'
                    Layout = tlCenter
                  end
                  object EdOutPort: TEdit
                    Left = 230
                    Top = 5
                    Width = 602
                    Height = 23
                    Align = alRight
                    TabOrder = 0
                  end
                end
                object PnlOutboundRowTls: TPanel
                  Left = 0
                  Top = 68
                  Width = 842
                  Height = 34
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 2
                  ExplicitWidth = 848
                  object LblTlsMode: TLabel
                    Left = 0
                    Top = 0
                    Width = 78
                    Height = 34
                    Align = alLeft
                    Alignment = taRightJustify
                    Caption = 'TLS/SSL mode:'
                    Layout = tlCenter
                    ExplicitHeight = 15
                  end
                  object CbTlsMode: TComboBox
                    Left = 240
                    Top = 0
                    Width = 602
                    Height = 23
                    Align = alRight
                    Style = csDropDownList
                    ItemIndex = 0
                    TabOrder = 0
                    Text = 'PLAIN'
                    Items.Strings = (
                      'PLAIN'
                      'STARTTLS'
                      'SSL_IMPLICIT')
                  end
                end
                object PnlOutboundRowWorkers: TPanel
                  Left = 0
                  Top = 102
                  Width = 842
                  Height = 34
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 3
                  ExplicitWidth = 848
                  object LblWorkers: TLabel
                    Left = 174
                    Top = 0
                    Width = 46
                    Height = 15
                    Align = alLeft
                    Alignment = taRightJustify
                    Caption = 'Workers:'
                    Layout = tlCenter
                  end
                  object EdWorkers: TEdit
                    Left = 230
                    Top = 5
                    Width = 602
                    Height = 23
                    Align = alRight
                    TabOrder = 0
                  end
                end
                object PnlOutboundRowWorkersHelp: TPanel
                  Left = 0
                  Top = 136
                  Width = 842
                  Height = 28
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 4
                  ExplicitWidth = 848
                  object LblWorkersHelp: TLabel
                    Left = 16
                    Top = 6
                    Width = 816
                    Height = 18
                    AutoSize = False
                    Caption = 
                      'Number of parallel sender threads. Increase carefully according ' +
                      'to SMTP provider limits.'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clGrayText
                    Font.Height = -11
                    Font.Name = 'Segoe UI'
                    Font.Style = []
                    ParentFont = False
                    WordWrap = True
                  end
                end
              end
            end
            object GbOutboundAuth: TGroupBox
              Left = 0
              Top = 206
              Width = 846
              Height = 204
              Align = alTop
              Caption = 'Outbound auth credentials'
              TabOrder = 1
              object PnlOutboundAuthBody: TPanel
                Left = 2
                Top = 17
                Width = 848
                Height = 191
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                object PnlOutboundAuthRowUser: TPanel
                  Left = 0
                  Top = 0
                  Width = 848
                  Height = 34
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 0
                  object LblOutboundAuthUser: TLabel
                    Left = 164
                    Top = 0
                    Width = 56
                    Height = 15
                    Align = alLeft
                    Alignment = taRightJustify
                    Caption = 'Username:'
                    Layout = tlCenter
                  end
                  object EdOutboundAuthUser: TEdit
                    Left = 230
                    Top = 5
                    Width = 602
                    Height = 23
                    Align = alRight
                    TabOrder = 0
                  end
                end
                object PnlOutboundAuthRowPass: TPanel
                  Left = 0
                  Top = 34
                  Width = 848
                  Height = 34
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 1
                  object LblOutboundAuthPassword: TLabel
                    Left = 167
                    Top = 0
                    Width = 53
                    Height = 15
                    Align = alLeft
                    Alignment = taRightJustify
                    Caption = 'Password:'
                    Layout = tlCenter
                  end
                  object EdOutboundAuthPassword: TEdit
                    Left = 230
                    Top = 5
                    Width = 602
                    Height = 23
                    Align = alRight
                    PasswordChar = '*'
                    TabOrder = 0
                  end
                end
              end
            end
          end
        end
        object PnlConfigBottom: TPanel
          Left = 0
          Top = 500
          Width = 1412
          Height = 250
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object SplConfigQueue: TSplitter
            Left = 360
            Top = 0
            Width = 6
            Height = 250
            Enabled = False
            MinSize = 240
            Visible = False
          end
          object SplConfigUI: TSplitter
            Left = 726
            Top = 0
            Width = 6
            Height = 250
            Enabled = False
            MinSize = 240
            Visible = False
          end
          object GbQueueConfig: TGroupBox
            Left = 0
            Top = 0
            Width = 360
            Height = 250
            Align = alLeft
            Caption = 'Queue'
            TabOrder = 0
            object PnlQueueConfigBody: TPanel
              Left = 2
              Top = 17
              Width = 356
              Height = 231
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              object PnlQueueRowPath: TPanel
                Left = 0
                Top = 0
                Width = 356
                Height = 34
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 0
                object LblQueuePathCaption: TLabel
                  Left = 143
                  Top = 0
                  Width = 27
                  Height = 15
                  Align = alLeft
                  Alignment = taRightJustify
                  Caption = 'Path:'
                  Layout = tlCenter
                end
                object EdQueuePath: TEdit
                  Left = 180
                  Top = 5
                  Width = 166
                  Height = 23
                  Align = alRight
                  TabOrder = 0
                end
              end
              object PnlQueueRowMaxItems: TPanel
                Left = 0
                Top = 34
                Width = 356
                Height = 34
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 1
                object LblQueueMaxItemsCaption: TLabel
                  Left = 76
                  Top = 0
                  Width = 94
                  Height = 15
                  Align = alLeft
                  Alignment = taRightJustify
                  Caption = 'Max queue items:'
                  Layout = tlCenter
                end
                object EdQueueMaxItems: TEdit
                  Left = 180
                  Top = 5
                  Width = 166
                  Height = 23
                  Align = alRight
                  TabOrder = 0
                end
              end
              object PnlQueueRowMaxBytes: TPanel
                Left = 0
                Top = 68
                Width = 356
                Height = 34
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 2
                object LblQueueMaxBytesCaption: TLabel
                  Left = 48
                  Top = 0
                  Width = 122
                  Height = 15
                  Align = alLeft
                  Alignment = taRightJustify
                  Caption = 'Max queue bytes (MB):'
                  Layout = tlCenter
                end
                object EdQueueMaxBytesMB: TEdit
                  Left = 180
                  Top = 5
                  Width = 166
                  Height = 23
                  Align = alRight
                  TabOrder = 0
                end
              end
              object PnlQueueRowStale: TPanel
                Left = 0
                Top = 102
                Width = 356
                Height = 34
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 3
                object LblQueueStaleCaption: TLabel
                  Left = 36
                  Top = 0
                  Width = 134
                  Height = 15
                  Align = alLeft
                  Alignment = taRightJustify
                  Caption = 'In-flight stale timeout (s):'
                  Layout = tlCenter
                end
                object EdQueueInFlightStaleSec: TEdit
                  Left = 180
                  Top = 5
                  Width = 166
                  Height = 23
                  Align = alRight
                  TabOrder = 0
                end
              end
            end
          end
          object GbUiConfig: TGroupBox
            Left = 366
            Top = 0
            Width = 360
            Height = 250
            Align = alLeft
            Caption = 'Config'
            TabOrder = 1
            object PnlUiConfigBody: TPanel
              Left = 2
              Top = 17
              Width = 356
              Height = 231
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              object PnlUiRowLanguage: TPanel
                Left = 0
                Top = 0
                Width = 356
                Height = 34
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 0
                object LblLanguageCaption: TLabel
                  Left = 115
                  Top = 0
                  Width = 55
                  Height = 15
                  Align = alLeft
                  Alignment = taRightJustify
                  Caption = 'Language:'
                  Layout = tlCenter
                end
                object CbLanguage: TComboBox
                  Left = 180
                  Top = 5
                  Width = 166
                  Height = 23
                  Align = alRight
                  Style = csDropDownList
                  TabOrder = 0
                end
              end
              object PnlUiRowDetailLogging: TPanel
                Left = 0
                Top = 34
                Width = 356
                Height = 34
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 1
                object CbDetailLogging: TCheckBox
                  Left = 16
                  Top = 8
                  Width = 330
                  Height = 17
                  Align = alRight
                  Caption = 'Enable detail logging'
                  TabOrder = 0
                end
              end
              object PnlUiRowDetailLoggingHelp: TPanel
                Left = 0
                Top = 68
                Width = 356
                Height = 34
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 2
                object LblDetailLoggingHelp: TLabel
                  Left = 16
                  Top = 4
                  Width = 330
                  Height = 26
                  AutoSize = False
                  Caption = 'Adds SMTP trace lines: C:, S:, C-DATA:, EVT:'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clGrayText
                  Font.Height = -11
                  Font.Name = 'Segoe UI'
                  Font.Style = []
                  ParentFont = False
                  WordWrap = True
                end
              end
              object PnlUiRowLogTail: TPanel
                Left = 0
                Top = 102
                Width = 356
                Height = 34
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 3
                object LblLogTailLinesCaption: TLabel
                  Left = 101
                  Top = 0
                  Width = 69
                  Height = 15
                  Align = alLeft
                  Alignment = taRightJustify
                  Caption = 'Log tail lines:'
                  Layout = tlCenter
                end
                object EdLogTailLines: TEdit
                  Left = 180
                  Top = 5
                  Width = 166
                  Height = 23
                  Align = alRight
                  TabOrder = 0
                end
              end
              object PnlUiRowStatsRefresh: TPanel
                Left = 0
                Top = 136
                Width = 356
                Height = 34
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 4
                object LblStatsRefreshCaption: TLabel
                  Left = 76
                  Top = 0
                  Width = 94
                  Height = 15
                  Align = alLeft
                  Alignment = taRightJustify
                  Caption = 'Stats refresh (ms):'
                  Layout = tlCenter
                end
                object EdStatsRefreshMs: TEdit
                  Left = 180
                  Top = 5
                  Width = 166
                  Height = 23
                  Align = alRight
                  TabOrder = 0
                end
              end
            end
          end
          object GbRetry: TGroupBox
            Left = 732
            Top = 0
            Width = 680
            Height = 250
            Align = alClient
            Caption = 'Retry'
            TabOrder = 2
            object PnlRetryBody: TPanel
              Left = 2
              Top = 17
              Width = 688
              Height = 231
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              object PnlRetryRowAttempts: TPanel
                Left = 0
                Top = 0
                Width = 688
                Height = 34
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 0
                object LblRetryAttemptsCaption: TLabel
                  Left = 204
                  Top = 0
                  Width = 76
                  Height = 15
                  Align = alLeft
                  Alignment = taRightJustify
                  Caption = 'Max attempts:'
                  Layout = tlCenter
                end
                object EdRetryMaxAttempts: TEdit
                  Left = 290
                  Top = 5
                  Width = 388
                  Height = 23
                  Align = alRight
                  TabOrder = 0
                end
              end
              object PnlRetryRowBaseDelay: TPanel
                Left = 0
                Top = 34
                Width = 688
                Height = 34
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 1
                object LblRetryBaseDelayCaption: TLabel
                  Left = 149
                  Top = 0
                  Width = 131
                  Height = 15
                  Align = alLeft
                  Alignment = taRightJustify
                  Caption = 'Base delay between tries:'
                  Layout = tlCenter
                end
                object EdRetryBaseDelaySec: TEdit
                  Left = 290
                  Top = 5
                  Width = 388
                  Height = 23
                  Align = alRight
                  TabOrder = 0
                end
              end
              object PnlRetryRowMaxDelay: TPanel
                Left = 0
                Top = 68
                Width = 688
                Height = 34
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 2
                object LblRetryMaxDelayCaption: TLabel
                  Left = 175
                  Top = 0
                  Width = 105
                  Height = 15
                  Align = alLeft
                  Alignment = taRightJustify
                  Caption = 'Maximum delay (s):'
                  Layout = tlCenter
                end
                object EdRetryMaxDelaySec: TEdit
                  Left = 290
                  Top = 5
                  Width = 388
                  Height = 23
                  Align = alRight
                  TabOrder = 0
                end
              end
              object PnlRetryRowJitter: TPanel
                Left = 0
                Top = 102
                Width = 688
                Height = 34
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 3
                object LblRetryJitterCaption: TLabel
                  Left = 231
                  Top = 0
                  Width = 49
                  Height = 15
                  Align = alLeft
                  Alignment = taRightJustify
                  Caption = 'Jitter (%):'
                  Layout = tlCenter
                end
                object EdRetryJitterPct: TEdit
                  Left = 290
                  Top = 5
                  Width = 388
                  Height = 23
                  Align = alRight
                  TabOrder = 0
                end
              end
              object PnlRetryRowJitterHelp: TPanel
                Left = 0
                Top = 136
                Width = 688
                Height = 30
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 4
                object LblRetryJitterHelp: TLabel
                  Left = 16
                  Top = 6
                  Width = 662
                  Height = 20
                  AutoSize = False
                  Caption = 
                    'Jitter adds random delay variation to avoid retry bursts and syn' +
                    'chronized reconnect storms.'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clGrayText
                  Font.Height = -11
                  Font.Name = 'Segoe UI'
                  Font.Style = []
                  ParentFont = False
                  WordWrap = True
                end
              end
            end
          end
        end
        object PnlConfigButtons: TPanel
          Left = 0
          Top = 750
          Width = 1412
          Height = 86
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 2
          object LblAutoSaveNote: TLabel
            Left = 16
            Top = 50
            Width = 206
            Height = 15
            Caption = 'All changes will be saved automatically'
          end
          object BtnRestoreDefaults: TButton
            Left = 16
            Top = 12
            Width = 200
            Height = 30
            Caption = 'Reset to defaults'
            TabOrder = 0
            OnClick = OnRestoreDefaults
          end
        end
      end
    end
    object TabLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 2
      object PnlLogTop: TPanel
        Left = 0
        Top = 0
        Width = 1412
        Height = 44
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object BtnClearLog: TButton
          Left = 16
          Top = 8
          Width = 120
          Height = 27
          Caption = 'Clear log'
          TabOrder = 0
          OnClick = OnClearLog
        end
        object BtnCopyLog: TButton
          Left = 144
          Top = 8
          Width = 120
          Height = 27
          Caption = 'Copy all'
          TabOrder = 1
          OnClick = OnCopyLog
        end
      end
      object MemoLog: TMemo
        Left = 0
        Top = 44
        Width = 1412
        Height = 825
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
        WordWrap = False
      end
    end
  end
  object StatusBarMain: TStatusBar
    Left = 0
    Top = 881
    Width = 1420
    Height = 19
    Cursor = crHandPoint
    Panels = <
      item
        Width = 220
      end
      item
        Width = 140
      end>
    OnMouseDown = OnStatusBarMainMouseDown
  end
end
