object Form1: TForm1
  Left = 192
  Top = 103
  Width = 1032
  Height = 656
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = Form1Create
  OnDestroy = Form1Destroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 289
    Top = 41
    Height = 588
  end
  object Splitter2: TSplitter
    Left = 775
    Top = 41
    Height = 588
    Align = alRight
  end
  object lbAllUnits: TListBox
    Left = 0
    Top = 41
    Width = 289
    Height = 588
    Align = alLeft
    ItemHeight = 13
    Items.Strings = (
      'd:\andrey\delphi\doc\BusinessLogic\API\uDocIn_Search.pas'
      'd:\andrey\delphi\doc\BusinessLogic\API\uDocOut_Search.pas'
      'd:\andrey\delphi\doc\BusinessLogic\API\uDocProt_Search.pas'
      'd:\andrey\delphi\doc\BusinessLogic\API\uDocsAPI.pas'
      'd:\andrey\delphi\doc\BusinessLogic\API\uDocsInApi.pas'
      'd:\andrey\delphi\doc\BusinessLogic\API\uDocsOutApi.pas'
      'd:\andrey\delphi\doc\BusinessLogic\API\uDocsProtApi.pas'
      'd:\andrey\delphi\doc\BusinessLogic\API\uLinksApi.pas'
      'd:\andrey\delphi\doc\BusinessLogic\API\uRouteApi.pas'
      'd:\andrey\delphi\doc\BusinessLogic\API\uSearchAndFilterApi.pas'
      'd:\andrey\delphi\doc\BusinessLogic\API\uTasksApi.pas'
      'd:\andrey\delphi\doc\BusinessLogic\Commands\genCommands.pas'
      'd:\andrey\delphi\doc\BusinessLogic\Commands\uCommands.pas'
      'd:\andrey\delphi\doc\BusinessLogic\Commands\uCommands_Docs.pas'
      
        'd:\andrey\delphi\doc\BusinessLogic\Commands\uCommands_Filters.pa' +
        's'
      
        'd:\andrey\delphi\doc\BusinessLogic\Commands\uCommands_Filters_In' +
        '.pas'
      
        'd:\andrey\delphi\doc\BusinessLogic\Commands\uCommands_Filters_Ou' +
        't.pas'
      
        'd:\andrey\delphi\doc\BusinessLogic\Commands\uCommands_Filters_Pr' +
        'ot.pas'
      
        'd:\andrey\delphi\doc\BusinessLogic\Commands\uCommands_Filters_Ro' +
        'ute.pas'
      'd:\andrey\delphi\doc\BusinessLogic\Commands\uCommands_In.pas'
      'd:\andrey\delphi\doc\BusinessLogic\Commands\uCommands_Out.pas'
      'd:\andrey\delphi\doc\BusinessLogic\Commands\uCommands_Prot.pas'
      'd:\andrey\delphi\doc\BusinessLogic\Rules\uDocsInRules.pas'
      'd:\andrey\delphi\doc\BusinessLogic\Rules\uDocsOutRules.pas'
      'd:\andrey\delphi\doc\BusinessLogic\Rules\uDocsProtRules.pas'
      'd:\andrey\delphi\doc\BusinessLogic\Rules\uDocsRules.pas'
      'd:\andrey\delphi\doc\BusinessLogic\Rules\uTasksRules.pas'
      'd:\andrey\delphi\doc\DOMAIN\AppInfrastructure\uExceptions.pas'
      'd:\andrey\delphi\doc\DOMAIN\AppInfrastructure\uIoC.pas'
      'd:\andrey\delphi\doc\DOMAIN\AppInfrastructure\uNull.pas'
      'd:\andrey\delphi\doc\DOMAIN\AppInfrastructure\uRegClasses.pas'
      
        'd:\andrey\delphi\doc\DOMAIN\AppInfrastructure\uSingleInstance.pa' +
        's'
      'd:\andrey\delphi\doc\DOMAIN\DTO\uDTO_Domain_Doc.pas'
      'd:\andrey\delphi\doc\DOMAIN\DTO\uDTO_Domain_DocIn.pas'
      'd:\andrey\delphi\doc\DOMAIN\DTO\uDTO_Domain_DocOut.pas'
      'd:\andrey\delphi\doc\DOMAIN\DTO\uDTO_Domain_DocProt.pas'
      'd:\andrey\delphi\doc\DOMAIN\DTO\uDTO_Domain_DocRoute.pas'
      'd:\andrey\delphi\doc\DOMAIN\DTO\uDTO_Domain_Links.pas'
      'd:\andrey\delphi\doc\DOMAIN\DTO\uDTO_Domain_Misc.pas'
      'd:\andrey\delphi\doc\DOMAIN\DTO\uDTO_Domain_Tasks.pas'
      'd:\andrey\delphi\doc\DOMAIN\Entities\Base\uDoc.pas'
      'd:\andrey\delphi\doc\DOMAIN\Entities\Base\uJournal.pas'
      'd:\andrey\delphi\doc\DOMAIN\Entities\Base\uTasksDoc.pas'
      'd:\andrey\delphi\doc\DOMAIN\Entities\In\uDocIn.pas'
      'd:\andrey\delphi\doc\DOMAIN\Entities\In\uJournal_In.pas'
      'd:\andrey\delphi\doc\DOMAIN\Entities\Out\uDocOut.pas'
      'd:\andrey\delphi\doc\DOMAIN\Entities\Out\uJournal_Out.pas'
      'd:\andrey\delphi\doc\DOMAIN\Entities\Prot\uDocProt.pas'
      'd:\andrey\delphi\doc\DOMAIN\Entities\Prot\uJournal_Prot.pas'
      'd:\andrey\delphi\doc\DOMAIN\Entities\uControlUser.pas'
      'd:\andrey\delphi\doc\DOMAIN\Entities\uTask.pas'
      'd:\andrey\delphi\doc\DOMAIN\Entities\uUser.pas'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_DocIn_Itf.p' +
        'as'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_DocOut_Itf.' +
        'pas'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_DocProt_Itf' +
        '.pas'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_Docs_Itf.pa' +
        's'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_DocsByNoPar' +
        't_Itf.pas'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_Journals_It' +
        'f.pas'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_Links_Itf.p' +
        'as'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_Route_Itf.p' +
        'as'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_Signed_Itf.' +
        'pas'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_StringLists' +
        '_Itf.pas'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_Talon_Itf.p' +
        'as'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_Tasks_Itf.p' +
        'as'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_Unsorted_It' +
        'f.pas'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_Users_Itf.p' +
        'as'
      
        'd:\andrey\delphi\doc\DOMAIN\Repositories\uRepository_Vars_Itf.pa' +
        's'
      'd:\andrey\delphi\doc\DOMAIN\uCore.pas'
      'd:\andrey\delphi\doc\DOMAIN\uDataSourceFilters.pas'
      'd:\andrey\delphi\doc\DOMAIN\uDocsFactory.pas'
      'd:\andrey\delphi\doc\DOMAIN\uDocumentFileProvider_Itf.pas'
      'd:\andrey\delphi\doc\DOMAIN\uDOMAIN.pas'
      'd:\andrey\delphi\doc\DOMAIN\uFileSystemPaths_Itf.pas'
      'd:\andrey\delphi\doc\DOMAIN\uFilters.pas'
      'd:\andrey\delphi\doc\DOMAIN\uLazyVars.pas'
      'd:\andrey\delphi\doc\DOMAIN\uNotifications_Domain.pas'
      'd:\andrey\delphi\doc\DOMAIN\uServerFiles_Itf.pas'
      'd:\andrey\delphi\doc\FormApplets\uEmployeesReport.pas'
      'd:\andrey\delphi\doc\FormApplets\uMissedDocsReport.pas'
      'd:\andrey\delphi\doc\Infrastructure\Facades\uNSFacade.pas'
      'd:\andrey\delphi\doc\Infrastructure\Facades\uScanFacade.pas'
      'd:\andrey\delphi\doc\Infrastructure\uAppSettings_Impl.pas'
      
        'd:\andrey\delphi\doc\Infrastructure\uDocumentFileProvider_Impl.p' +
        'as'
      'd:\andrey\delphi\doc\Infrastructure\uFileSystemPaths_Impl.pas'
      'd:\andrey\delphi\doc\Infrastructure\uUtils.pas'
      'd:\andrey\delphi\doc\PL\DocReaders\uDocReader_Base.pas'
      'd:\andrey\delphi\doc\PL\DocReaders\uDocReader_In.pas'
      'd:\andrey\delphi\doc\PL\DocReaders\uDocReader_Out.pas'
      'd:\andrey\delphi\doc\PL\DocReaders\uDocReader_Prot.pas'
      'd:\andrey\delphi\doc\PL\FileSystem\uServerFiles_Impl.pas'
      'd:\andrey\delphi\doc\PL\Repositories\uRepository_DocIn_Impl.pas'
      'd:\andrey\delphi\doc\PL\Repositories\uRepository_DocOut_Impl.pas'
      
        'd:\andrey\delphi\doc\PL\Repositories\uRepository_DocProt_Impl.pa' +
        's'
      'd:\andrey\delphi\doc\PL\Repositories\uRepository_Docs_Impl.pas'
      
        'd:\andrey\delphi\doc\PL\Repositories\uRepository_DocsByNoPart_Im' +
        'pl.pas'
      
        'd:\andrey\delphi\doc\PL\Repositories\uRepository_Journals_Impl.p' +
        'as'
      'd:\andrey\delphi\doc\PL\Repositories\uRepository_Links_Impl.pas'
      'd:\andrey\delphi\doc\PL\Repositories\uRepository_Route_Impl.pas'
      'd:\andrey\delphi\doc\PL\Repositories\uRepository_Signed_Impl.pas'
      
        'd:\andrey\delphi\doc\PL\Repositories\uRepository_StringLists_Imp' +
        'l.pas'
      'd:\andrey\delphi\doc\PL\Repositories\uRepository_Talon_Impl.pas'
      'd:\andrey\delphi\doc\PL\Repositories\uRepository_Tasks_Impl.pas'
      
        'd:\andrey\delphi\doc\PL\Repositories\uRepository_Unsorted_Impl.p' +
        'as'
      'd:\andrey\delphi\doc\PL\Repositories\uRepository_Users_Impl.pas'
      'd:\andrey\delphi\doc\PL\Repositories\uRepository_Vars_Impl.pas'
      'd:\andrey\delphi\doc\PL\uLazyVarsDB.pas'
      'd:\andrey\delphi\doc\PL\uPL.pas'
      'd:\andrey\delphi\doc\PL\uSingleRecordsetRepository.pas'
      'd:\andrey\delphi\doc\UI\Actions\uActions.pas'
      'd:\andrey\delphi\doc\UI\Actions\uActions_Docs.pas'
      'd:\andrey\delphi\doc\UI\Actions\uActions_Docs_In.pas'
      'd:\andrey\delphi\doc\UI\Actions\uActions_Docs_Out.pas'
      'd:\andrey\delphi\doc\UI\Actions\uActions_Docs_Prot.pas'
      'd:\andrey\delphi\doc\UI\Actions\uActions_Docs_Route.pas'
      'd:\andrey\delphi\doc\UI\Actions\uActions_Links.pas'
      'd:\andrey\delphi\doc\UI\Actions\uActions_Search.pas'
      'd:\andrey\delphi\doc\UI\Actions\uActions_Tasks.pas'
      'd:\andrey\delphi\doc\UI\Actions\uActionsInfo.pas'
      'd:\andrey\delphi\doc\UI\API\uDocsApiUI.pas'
      'd:\andrey\delphi\doc\UI\API\uDocsInApiUI.pas'
      'd:\andrey\delphi\doc\UI\API\uDocsOutApiUI.pas'
      'd:\andrey\delphi\doc\UI\API\uDocsProtApiUI.pas'
      'd:\andrey\delphi\doc\UI\API\uLinksApiUI.pas'
      'd:\andrey\delphi\doc\UI\API\uRouteApiUI.pas'
      'd:\andrey\delphi\doc\UI\API\uSearchAndFilterApiUI.pas'
      'd:\andrey\delphi\doc\UI\API\uTasksApiUI.pas'
      'd:\andrey\delphi\doc\UI\API\uToolBarApiUI.pas'
      'd:\andrey\delphi\doc\UI\Applets\Base\uApplet_Controller_Base.pas'
      'd:\andrey\delphi\doc\UI\Applets\Base\uApplet_View_Base.pas'
      'd:\andrey\delphi\doc\UI\Applets\Base\uAppletInfo_Base.pas'
      'd:\andrey\delphi\doc\UI\Applets\Base\uApplets_ViewHandler.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SearchResults\uApplet_Controller' +
        '_SearchResults.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SearchResults\uApplet_Model_Sear' +
        'chResults.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SearchResults\uApplet_Scope_Sear' +
        'chResults.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SearchResults\uApplet_View_Searc' +
        'hResults.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SearchResults\uAppletInfo_Search' +
        'Results.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SignedJournal\uApplet_Controller' +
        '_SignedJournal.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SignedJournal\uApplet_Model_Sign' +
        'edJournal.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SignedJournal\uApplet_Scope_Sign' +
        'edJournal.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SignedJournal\uApplet_View_Signe' +
        'dJournal.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SignedJournal\uAppletInfo_Signed' +
        'Journal.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SingleJournal\uApplet_Controller' +
        '_SingleJournal.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SingleJournal\uApplet_Model_Sing' +
        'leJournal.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SingleJournal\uApplet_Scope_Sing' +
        'leJournal.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SingleJournal\uApplet_Scope_Sing' +
        'leJournal_Tasks.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SingleJournal\uApplet_View_Singl' +
        'eJournal.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SingleJournal\uAppletInfo_Single' +
        'Journal.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SingleJournal\uAppletInfo_Single' +
        'Journal_In.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SingleJournal\uAppletInfo_Single' +
        'Journal_Out.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\SingleJournal\uAppletInfo_Single' +
        'Journal_Prot.pas'
      
        'd:\andrey\delphi\doc\UI\Applets\Talon\uApplet_Controller_Talon.p' +
        'as'
      'd:\andrey\delphi\doc\UI\Applets\Talon\uApplet_Model_Talon.pas'
      'd:\andrey\delphi\doc\UI\Applets\Talon\uApplet_View_Talon.pas'
      'd:\andrey\delphi\doc\UI\Applets\Talon\uAppletInfo_Talon.pas'
      'd:\andrey\delphi\doc\UI\AppletsPane\uAppletsPane_Controller.pas'
      'd:\andrey\delphi\doc\UI\AppletsPane\uAppletsPane_Models.pas'
      'd:\andrey\delphi\doc\UI\AppletsPane\uAppletsPane_View.pas'
      
        'd:\andrey\delphi\doc\UI\BottomFrames\Base\uBottomFrame_Controlle' +
        'r_Base.pas'
      
        'd:\andrey\delphi\doc\UI\BottomFrames\Base\uBottomFrame_View_Base' +
        '.pas'
      
        'd:\andrey\delphi\doc\UI\BottomFrames\Links\uBottomFrame_Controll' +
        'er_Links.pas'
      
        'd:\andrey\delphi\doc\UI\BottomFrames\Links\uBottomFrame_Model_Li' +
        'nks.pas'
      
        'd:\andrey\delphi\doc\UI\BottomFrames\Links\uBottomFrame_Scope_Li' +
        'nks.pas'
      
        'd:\andrey\delphi\doc\UI\BottomFrames\Links\uBottomFrame_View_Lin' +
        'ks.pas'
      
        'd:\andrey\delphi\doc\UI\BottomFrames\Tasks\uBottomFrame_Controll' +
        'er_Tasks.pas'
      
        'd:\andrey\delphi\doc\UI\BottomFrames\Tasks\uBottomFrame_Model_Ta' +
        'sks.pas'
      
        'd:\andrey\delphi\doc\UI\BottomFrames\Tasks\uBottomFrame_Scope_Ta' +
        'sks.pas'
      
        'd:\andrey\delphi\doc\UI\BottomFrames\Tasks\uBottomFrame_View_Tas' +
        'ks.pas'
      
        'd:\andrey\delphi\doc\UI\BottomFrames\uBottomFrames_ViewHandler.p' +
        'as'
      'd:\andrey\delphi\doc\UI\Controls\uBootstrapStatusFrm.pas'
      'd:\andrey\delphi\doc\UI\Controls\uCheckConnectionFrm.pas'
      'd:\andrey\delphi\doc\UI\Controls\uSelectLinkFrame.pas'
      'd:\andrey\delphi\doc\UI\Controls\uTaskDateFrame.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uAddLinkDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uAddTaskDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uAddUserDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uControlDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uDateRangeDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uDocInDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uDocOutDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uDocProtDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uDocTypeAndDocNoDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uFilterDialogs_Model.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uMemoDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uMoveDocDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uRushDocsReportDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uSendOnSignDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uSignDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uStrDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uStrListDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uTaskDateDlg.pas'
      'd:\andrey\delphi\doc\UI\Dialogs\uUserListDlg.pas'
      'd:\andrey\delphi\doc\UI\ListViews\Controls\uBaseListView.pas'
      
        'd:\andrey\delphi\doc\UI\ListViews\Controls\uColumnSetListView.pa' +
        's'
      'd:\andrey\delphi\doc\UI\ListViews\Controls\uJournalListView.pas'
      'd:\andrey\delphi\doc\UI\ListViews\Controls\uLinksListView.pas'
      
        'd:\andrey\delphi\doc\UI\ListViews\Controls\uListViewColumnMenu.p' +
        'as'
      'd:\andrey\delphi\doc\UI\ListViews\Controls\uSignedListView.pas'
      'd:\andrey\delphi\doc\UI\ListViews\Controls\uTasksListView.pas'
      
        'd:\andrey\delphi\doc\UI\ListViews\Selection\uDocsSelection_Impl.' +
        'pas'
      
        'd:\andrey\delphi\doc\UI\ListViews\Selection\uDocsSelection_Itf.p' +
        'as'
      
        'd:\andrey\delphi\doc\UI\ListViews\Selection\uTasksSelection_Impl' +
        '.pas'
      
        'd:\andrey\delphi\doc\UI\ListViews\Selection\uTasksSelection_Itf.' +
        'pas'
      'd:\andrey\delphi\doc\UI\ListViews\uListViewBehavior.pas'
      'd:\andrey\delphi\doc\UI\ListViews\uUI_ListView.pas'
      'd:\andrey\delphi\doc\UI\MainWindow\uMainWindow_Controller.pas'
      'd:\andrey\delphi\doc\UI\MainWindow\uMainWindow_Itf.pas'
      'd:\andrey\delphi\doc\UI\MainWindow\uMainWindow_View.pas'
      'd:\andrey\delphi\doc\UI\Models\uListViewColumns.pas'
      'd:\andrey\delphi\doc\UI\Models\uModel_Base.pas'
      'd:\andrey\delphi\doc\UI\Models\uModel_Journal.pas'
      'd:\andrey\delphi\doc\UI\Models\uModel_Toolbar.pas'
      'd:\andrey\delphi\doc\UI\Models\uUI_Model_In.pas'
      'd:\andrey\delphi\doc\UI\Models\uUI_Model_Links.pas'
      'd:\andrey\delphi\doc\UI\Models\uUI_Model_Out.pas'
      'd:\andrey\delphi\doc\UI\Models\uUI_Model_Prot.pas'
      'd:\andrey\delphi\doc\UI\Models\uUI_Model_Tasks.pas'
      
        'd:\andrey\delphi\doc\UI\ScopeInterfaces\uScopeInterfaces_Docs.pa' +
        's'
      
        'd:\andrey\delphi\doc\UI\ScopeInterfaces\uScopeInterfaces_Links.p' +
        'as'
      
        'd:\andrey\delphi\doc\UI\ScopeInterfaces\uScopeInterfaces_Tasks.p' +
        'as'
      'd:\andrey\delphi\doc\UI\uDelayedRun.pas'
      'd:\andrey\delphi\doc\UI\uNotifications_UI.pas'
      'd:\andrey\delphi\doc\UI\uResources.pas'
      'd:\andrey\delphi\doc\UI\uTasksUI.pas'
      'd:\andrey\delphi\doc\UI\uUI.pas'
      'd:\andrey\delphi\doc\UI\ViewModels\uVM_Doc.pas'
      'd:\andrey\delphi\doc\UI\ViewModels\uVM_DocIn.pas'
      'd:\andrey\delphi\doc\UI\ViewModels\uVM_DocOut.pas'
      'd:\andrey\delphi\doc\UI\ViewModels\uVM_DocProt.pas'
      'd:\andrey\delphi\doc\UI\ViewModels\uVM_DocRoute.pas'
      'd:\andrey\delphi\doc\UI\ViewModels\uVM_Tasks.pas')
    TabOrder = 0
    OnDblClick = lbAllUnitsDblClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1024
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Rebuild'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 96
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Close editor'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object pgEditors: TPageControl
    Left = 292
    Top = 41
    Width = 483
    Height = 588
    Align = alClient
    TabOrder = 2
  end
  object Panel2: TPanel
    Left = 778
    Top = 41
    Width = 246
    Height = 588
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 3
    object Splitter3: TSplitter
      Left = 0
      Top = 485
      Width = 246
      Height = 3
      Cursor = crVSplit
      Align = alBottom
    end
    object Splitter4: TSplitter
      Left = 0
      Top = 352
      Width = 246
      Height = 3
      Cursor = crVSplit
      Align = alBottom
    end
    object tvClasses: TTreeView
      Left = 0
      Top = 153
      Width = 246
      Height = 199
      Align = alClient
      HideSelection = False
      Indent = 19
      ReadOnly = True
      TabOrder = 0
    end
    object mmErrors: TMemo
      Left = 0
      Top = 488
      Width = 246
      Height = 100
      Align = alBottom
      ScrollBars = ssBoth
      TabOrder = 1
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 246
      Height = 153
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 96
        Height = 13
        Caption = 'Search class or unit:'
      end
      object vtxtClasses: TVarEdit
        Left = 8
        Top = 24
        Width = 177
        Height = 21
        TabOrder = 0
        ListBox = vlbClasses
        OnCheckVariant = vtxtClassesCheckVariant
      end
      object vlbClasses: TVarListBox
        Left = 8
        Top = 45
        Width = 177
        Height = 97
        ItemHeight = 13
        TabOrder = 1
      end
    end
    object PageControl1: TPageControl
      Left = 0
      Top = 355
      Width = 246
      Height = 130
      ActivePage = TabSheet1
      Align = alBottom
      TabOrder = 3
      object TabSheet1: TTabSheet
        Caption = 'Interfaces'
        object tvInterfaces: TTreeView
          Left = 0
          Top = 0
          Width = 238
          Height = 102
          Align = alClient
          HideSelection = False
          Indent = 19
          ReadOnly = True
          TabOrder = 0
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Metaclasses'
        ImageIndex = 1
        object tvMetaclasses: TTreeView
          Left = 0
          Top = 0
          Width = 238
          Height = 102
          Align = alClient
          HideSelection = False
          Indent = 19
          ReadOnly = True
          TabOrder = 0
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Singletons'
        ImageIndex = 2
        object tvSingletons: TTreeView
          Left = 0
          Top = 0
          Width = 238
          Height = 102
          Align = alClient
          HideSelection = False
          Indent = 19
          ReadOnly = True
          TabOrder = 0
        end
      end
    end
  end
end
