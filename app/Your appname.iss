  #define MyAppName "Your appname"
#define MyAppVersion "0.0.0"
#define MyAppExeName "Your appname.bat"
#define RVersion "3.6.0"
#define IncludeR false
#define PandocVersion "2.6"
#define IncludePandoc false
#define IncludeChrome false
#define RtoolsVersion "35"
#define IncludeRtools false
#define MyAppPublisher ""
#define MyAppURL ""

[Setup]
AppName = {#MyAppName}
AppId = {{3F1V318O-7TVE-Z69H-PHJT-0H0OXM55FYAO}
DefaultDirName = {userdocs}\{#MyAppName}
DefaultGroupName = {#MyAppName}
OutputDir = RInno_installer
OutputBaseFilename = setup_{#MyAppName}
SetupIconFile = setup.ico
AppVersion = {#MyAppVersion}
AppPublisher = {#MyAppPublisher}
AppPublisherURL = {#MyAppURL}
AppSupportURL = {#MyAppURL}
AppUpdatesURL = {#MyAppURL}
PrivilegesRequired = lowest
InfoBeforeFile = infobefore.txt
InfoAfterFile = infoafter.txt
Compression = lzma2/ultra64
SolidCompression = yes
ArchitecturesInstallIn64BitMode = x64

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\default.ico"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commonprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\default.ico"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon; IconFilename: "{app}\default.ico"

[Files]
Source: "LICENSE"; Flags: dontcopy noencryption
Source: "{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion
#if IncludeR
    Source: "R-{#RVersion}-win.exe"; DestDir: "{tmp}"; Check: RNeeded
#endif
#if IncludePandoc
    Source: "pandoc-{#PandocVersion}-windows.msi"; DestDir: "{tmp}"; Check: PandocNeeded
#endif
#if IncludeChrome
    Source: "chrome_installer.exe"; DestDir: "{tmp}"; Check: ChromeNeeded
#endif
#if IncludeRtools
    Source: "Rtools{#RtoolsVersion}.exe"; DestDir: "{tmp}";
#endif
Source: "default.ico"; DestDir: "{app}"; Flags: ignoreversion;
Source: "LICENSE"; DestDir: "{app}"; Flags: ignoreversion;
Source: "myapp.bat"; DestDir: "{app}"; Flags: ignoreversion;
Source: "server.R"; DestDir: "{app}"; Flags: ignoreversion;
Source: "setup.ico"; DestDir: "{app}"; Flags: ignoreversion;
Source: "ui.R"; DestDir: "{app}"; Flags: ignoreversion;
Source: "Your appname.bat"; DestDir: "{app}"; Flags: ignoreversion;
Source: "bin/BH_1.69.0-1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/crayon_1.3.4.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/digest_0.6.20.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/htmltools_0.3.6.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/httpuv_1.5.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/jsonlite_1.6.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/later_0.8.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/magrittr_1.5.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/mime_0.7.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/promises_1.0.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/R6_2.4.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/Rcpp_1.0.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/rlang_0.4.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/shiny_1.3.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/sourcetools_0.1.7.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/xtable_1.8-4.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "data/most_recent_etl.txt"; DestDir: "{app}\data"; Flags: ignoreversion;
Source: "html/assumption_curve_info.html"; DestDir: "{app}\html"; Flags: ignoreversion;
Source: "utils/config.cfg"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils/ensure.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils/get_app_from_app_url.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils/launch_app.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils/package_manager.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils/wsf/js/JSON.minify.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils/wsf/js/json2.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils/wsf/js/run.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils/wsf/run.wsf"; DestDir: "{app}\utils\wsf"; Flags: ignoreversion;
Source: "www/default.ico"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www/Flatly.css"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www/md5.js"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www/mycss.css"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www/output.png"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www/passwdInputBinding.js"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www/pwstyle.css"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www/RInno.png"; DestDir: "{app}\www"; Flags: ignoreversion;

  [Run]
  #if IncludeR
      Filename: "{tmp}\R-{#RVersion}-win.exe"; Parameters: "/SILENT"; WorkingDir: {tmp}; Check: RNeeded; Flags: skipifdoesntexist; StatusMsg: "Installing R if needed"
  #endif
  #if IncludePandoc
      Filename: "msiexec.exe"; Parameters: "/i ""{tmp}\pandoc-{#PandocVersion}-windows.msi"" /q"; WorkingDir: {tmp}; Check: PandocNeeded; Flags: skipifdoesntexist; StatusMsg: "Installing Pandoc if needed"
  #endif
  #if IncludeChrome
      Filename: "{tmp}\chrome_installer.exe"; Parameters: "/install"; WorkingDir: {tmp}; Check: ChromeNeeded; Flags: skipifdoesntexist; StatusMsg: "Installing Chrome if needed"
  #endif
  #if IncludeRtools
      Filename: "{tmp}\Rtools{#RtoolsVersion}.exe"; Parameters: "/VERYSILENT"; WorkingDir: {tmp}; Flags: skipifdoesntexist; StatusMsg: "Installing Rtools"
  #endif
  Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: shellexec postinstall skipifsilent


  [UninstallDelete]
Type: filesandordirs; Name: "{app}\library";
Type: filesandordirs; Name: "{app}\utils";
Type: filesandordirs; Name: "{app}\log";

[Code]
const
  ChromeRegKey = 'Software\Microsoft\Windows\CurrentVersion\App Paths\chrome.exe';
  IERegKey = 'Software\Microsoft\Windows\CurrentVersion\App Paths\IEXPLORE.EXE';
  FFRegKey = 'Software\Microsoft\Windows\CurrentVersion\App Paths\firefox.exe';
var
  RVersions: TStringList;
  RRegKey: string;
  RegPathsFile: string;
  SecondLicensePage: TOutputMsgMemoWizardPage;
  License2AcceptedRadio: TRadioButton;
  License2NotAcceptedRadio: TRadioButton;

// Is R installed?
function RDetected(): boolean;
var
    v: Integer;
    success: boolean;
begin
  success := false;
  for v := 0 to (RVersions.Count - 1) do
    begin
      if RegKeyExists(HKLM, 'Software\R-Core\R\' + RVersions[v]) or RegKeyExists(HKCU, 'Software\R-Core\R\' + RVersions[v]) then
      begin
        success := true;
        RRegKey := 'Software\R-Core\R\' + RVersions[v];
        break;
      end;
    end;
  Result := success;
end;

// If R is not detected, it is needed
function RNeeded(): boolean;
begin
  Result := not RDetected;
end;


// Is Chrome installed?
function ChromeDetected(): boolean;
var
    success: boolean;
begin
  success := RegKeyExists(HKLM, ChromeRegKey) or RegKeyExists(HKCU, ChromeRegKey);
  begin
    Result := success;
  end;
end;

// If Chrome is not detected, it is needed
function ChromeNeeded(): boolean;
begin
  Result := not ChromeDetected;
end;


// Registry path update function (adds an extra backslash for json)
function AddBackSlash(Value: string): string;
begin
  Result := Value;
  StringChangeEx(Result, '\', '\\', True);
end;


// Pandoc is stored in the System PATH
function PandocDetected(): boolean;
var
  PandocDir, Path: String;
begin
  Log('Checking for Pandoc in %PATH%');
  if RegQueryStringValue(HKEY_CURRENT_USER, 'Environment', 'Path', Path) then
  begin // Successfully read the value
    Log('HKCU\Environment\PATH = ' + Path);
    PandocDir := ExpandConstant('{localappdata}\Pandoc\');
    Log('Looking for Pandoc in %PATH%: ' + PandocDir + ' in ' + Path);
    if Pos(LowerCase(PandocDir), Lowercase(Path)) = 0 then
		begin
			Log('Did not find Pandoc in %PATH%');
			Result := False;
		end
    else
		begin
			Log('Found Pandoc in %PATH%');
			Result := True;
		end
  end
  else // The key probably doesn't exist
  begin
    Log('Could not access HKCU\Environment\PATH.');
    Result := False;
  end;
end;

// If Pandoc is not detected, it is needed
function PandocNeeded(): boolean;
begin
  Result := not PandocDetected;
end;

// Save installation paths
procedure SaveInstallationPaths();
var
  RPath, ChromePath, IEPath, FFPath, PandocPath: string;
begin
  RPath := '';
  ChromePath := '';
  IEPath := '';
  FFPath := '';
  PandocPath := ExpandConstant('{localappdata}\Pandoc\');
  RegPathsFile := ExpandConstant('{app}\utils\regpaths.json');

  if Length(RRegKey) = 0 then
    RDetected;

  // Create registry paths file
  SaveStringToFile(RegPathsFile, '{' + #13#10, True);

  // R RegPath
  if RegQueryStringValue(HKLM, RRegKey, 'InstallPath', RPath) or RegQueryStringValue(HKCU, RRegKey, 'InstallPath', RPath) then
    SaveStringToFile(RegPathsFile, '"r": "' + AddBackSlash(RPath) + '",' + #13#10, True)
  else
    SaveStringToFile(RegPathsFile, '"r": "none",' + #13#10, True);

  // Chrome RegPath
  if RegQueryStringValue(HKLM, ChromeRegKey, 'Path', ChromePath) or RegQueryStringValue(HKCU, ChromeRegKey, 'Path', ChromePath) then
    SaveStringToFile(RegPathsFile, '"chrome": "' + AddBackSlash(ChromePath) + '",' + #13#10, True)
  else
    SaveStringToFile(RegPathsFile, '"chrome": "none",' + #13#10, True);

  // Internet Explorer RegPath
  if RegQueryStringValue(HKLM, IERegKey, '', IEPath) then
    SaveStringToFile(RegPathsFile, '"ie": "' + AddBackSlash(IEPath) + '",' + #13#10, True)
  else
    SaveStringToFile(RegPathsFile, '"ie": "none",' + #13#10, True);

  // Firefox RegPath
  if RegQueryStringValue(HKLM, FFRegKey, 'Path', FFPath) then
    SaveStringToFile(RegPathsFile, '"ff": "' + AddBackSlash(FFPath) + '",' + #13#10, True)
  else
    SaveStringToFile(RegPathsFile, '"ff": "none",' + #13#10, True);

  // Pandoc RegPath
  // ** Last Line in json file (no trailing comma) **
  if PandocDetected() then
    SaveStringToFile(RegPathsFile, '"pandoc": "' + AddBackSlash(PandocPath) + '"' + #13#10, True)
  else
    SaveStringToFile(RegPathsFile, '"pandoc": "none"' + #13#10, True);

  SaveStringToFile(RegPathsFile, '}', True);
end;

// Pre- and post-installation actions
procedure CurStepChanged(CurStep: TSetupStep);
begin
  // Pre-installation actions
  if CurStep = ssInstall then
  begin
  #if IncludeR
  #else
    // With `CurStep = ssInstall` we can still `Abort` if R not included but needed
    if RNeeded then
    begin
      SuppressibleMsgBox(Format('Error: R >= %s not found',[RVersions[RVersions.Count - 1]]), mbError, MB_OK, MB_OK);
      Abort;
    end;
  #endif
  end;
  // Post-installation actions
  if CurStep = ssPostInstall then
  begin
    SaveInstallationPaths;
  end;
end;

// Add RInno's license to the installer
procedure CheckLicense2Accepted(Sender: TObject);
begin
  { Update Next button when user (un)accepts the license }
  WizardForm.NextButton.Enabled := License2AcceptedRadio.Checked;
end;

function CloneLicenseRadioButton(Source: TRadioButton): TRadioButton;
begin
  Result := TRadioButton.Create(WizardForm);
  Result.Parent := SecondLicensePage.Surface;
  Result.Caption := Source.Caption;
  Result.Left := Source.Left;
  Result.Top := Source.Top;
  Result.Width := Source.Width;
  Result.Height := Source.Height;
  Result.OnClick := @CheckLicense2Accepted;
end;

procedure CurPageChanged(CurPageID: Integer);
begin
  { Update Next button when user gets to second license page }
  if CurPageID = SecondLicensePage.ID then
  begin
    CheckLicense2Accepted(nil);
  end;
end;

procedure InitializeWizard();
var
  LicenseFileName: string;
  LicenseFilePath: string;
begin
  { Create second license page, with the same labels as the original license page }
  SecondLicensePage :=
    CreateOutputMsgMemoPage(
      wpLicense, SetupMessage(msgWizardLicense), SetupMessage(msgLicenseLabel),
      SetupMessage(msgLicenseLabel3), '');

  { Shrink license box to make space for radio buttons }
  SecondLicensePage.RichEditViewer.Height := WizardForm.LicenseMemo.Height;

  { Load license }
  { Loading ex-post, as Lines.LoadFromFile supports UTF-8, }
  { contrary to LoadStringFromFile. }
  LicenseFileName := 'LICENSE';
  ExtractTemporaryFile(LicenseFileName);
  LicenseFilePath := ExpandConstant('{tmp}\' + LicenseFileName);
  SecondLicensePage.RichEditViewer.Lines.LoadFromFile(LicenseFilePath);
  DeleteFile(LicenseFilePath);

  { Clone accept/do not accept radio buttons for the second license }
  License2AcceptedRadio := CloneLicenseRadioButton(WizardForm.LicenseAcceptedRadio);
  License2NotAcceptedRadio := CloneLicenseRadioButton(WizardForm.LicenseNotAcceptedRadio);

  { Initially not accepted }
  License2NotAcceptedRadio.Checked := True;

  // Initialize the values of supported versions
  RVersions := TStringList.Create; // Make a new TStringList object reference
  // Add strings to the StringList object
  RVersions.Add('3.6.1');
  RVersions.Add('3.6.1');
  RVersions.Add('3.6.0');

end;

// Procedure called by InnoSetup when it is closing
procedure DeinitializeSetup();
begin
  RVersions.Free;
end;
