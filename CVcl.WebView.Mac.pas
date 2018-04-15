{
  This file is part of CrossVCL WebBroser 
  Copyright (c) 2018 Eugene Kryukov

  The contents of this file are subject to the Mozilla Public License Version 2.0 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
  KIND, either express or implied. See the License for the specific language governing rights and
  limitations under the License.
}

unit CVcl.WebView.Mac;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, CVcl.WebView, Macapi.ObjectiveC, Macapi.Helpers, Macapi.Foundation,
  Macapi.AppKit, Macapi.WebKit;

type

  TMacWebView = class;

  WebFrameLoadDelegate = interface(IObjectiveC)
    ['{50FBA900-6373-4D55-89A0-21BC30D8B033}']
    procedure webViewDidFinishLoadForFrame(sender: WebView; didFinishLoadForFrame: WebFrame); cdecl;
    procedure webViewDidFailLoadWithErrorForFrame(sender: WebView; didFailLoadWithError: NSError; forFrame: WebFrame); cdecl;
  end;

  TCocoaDelegate = class(TOCLocal, WebFrameLoadDelegate)
  private
    FDelegate: IWebViewDelegate;
  public
    constructor Create(ADelegate: IWebViewDelegate);
    { WebFrameLoadDelegate }
    [MethodName('webView:didFinishLoadForFrame:')]
    procedure webViewDidFinishLoadForFrame(sender: WebView; didFinishLoadForFrame: WebFrame); cdecl;
    [MethodName('webView:didFailLoadWithError:forFrame:')]
    procedure webViewDidFailLoadWithErrorForFrame(sender: WebView; didFailLoadWithError: NSError; forFrame: WebFrame); cdecl;
  end;

  TMacWebView = class(TInterfacedObject, IWebView)
  private
    FWebView: WebView;
    FParentWnd: HWnd;
    FDelegate: IWebViewDelegate;
    FCocoaDelegate: TCocoaDelegate;
    { IWebView }
    procedure WebViewUpdateBounds;
    procedure WebViewNavigate(const URL: string);
    procedure WebViewGoForward;
    procedure WebViewGoBack;
    function WebGetHandle: Pointer;
  protected
  public
    constructor Create(AParentWnd: HWnd; ADelegate: IWebViewDelegate);
    destructor Destroy; override;
  end;

implementation

{ TCocoaDelegate }

constructor TCocoaDelegate.Create(ADelegate: IWebViewDelegate);
begin
  inherited Create;
  FDelegate := ADelegate;
end;

procedure TCocoaDelegate.webViewDidFailLoadWithErrorForFrame(sender: WebView;
  didFailLoadWithError: NSError; forFrame: WebFrame);
begin
//  FDelegate.FinishLoading;
end;

procedure TCocoaDelegate.webViewDidFinishLoadForFrame(sender: WebView; didFinishLoadForFrame: WebFrame);
begin
  FDelegate.FinishLoading;
end;

{ TMacWebView }

constructor TMacWebView.Create(AParentWnd: HWnd; ADelegate: IWebViewDelegate);
var
  WindowPtr, ContentViewPtr: Pointer;
  ContentView: NSView;
begin
  inherited Create;
  FDelegate := ADelegate;
  FParentWnd := AParentWnd;

  FWebView := TWebView.Create;

  FCocoaDelegate := TCocoaDelegate.Create(FDelegate);
  FWebView.setFrameLoadDelegate(FCocoaDelegate.GetObjectID);

  if GetNativeHandles(FParentWnd, WindowPtr, ContentViewPtr) then
  begin
    ContentView := TNSView.Wrap(ContentViewPtr);

    SetNativeParent((FWebView as ILocalObject).GetObjectID, FParentWnd);

    FWebView.setFrame(ContentView.bounds);
    FWebView.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
  end;
end;

destructor TMacWebView.Destroy;
begin
  FWebView.release;
  inherited;
end;

function TMacWebView.WebGetHandle: Pointer;
begin
  Result := FWebView;
end;

procedure TMacWebView.WebViewGoBack;
begin
  FWebView.goBack;
end;

procedure TMacWebView.WebViewGoForward;
begin
  FWebView.goForward;
end;

procedure TMacWebView.WebViewNavigate(const URL: string);
var
  URLRequest: NSURLRequest;
  URLStr: NSUrl;
begin
  URLStr := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(StrToNSStr(URL)));
  URLRequest := TNSURLRequest.Wrap(TNSURLRequest.OCClass.requestWithURL(URLStr));
  FWebView.mainFrame.loadRequest(URLRequest);
end;

procedure TMacWebView.WebViewUpdateBounds;
begin
  // Updated by Cocoa
end;

end.
