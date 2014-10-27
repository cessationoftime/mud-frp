Related Ticket:  http://trac.wxwidgets.org/ticket/8657

I am running WxWidgets 3.0.2, under Ubuntu 13.10.  I encountered a bug in my own program and was able to reproduce it by running one of the wxWidgets samples. 

To reproduce: 

run samples/notebook under GTK. In the File menu of the sample, choose Type>AuiNotebook.  Click the Veto tab, then click on some other tab, choose Yes when the veto dialog appears. You should have switched tabs.  Now click on any other tab.  You will receive the following Assertion Failed message:

{{{
../src/common/wincmn.cpp(3272): assert "!wxMouseCapture::IsInCaptureStack(this)" failed in CaptureMouse(): Recapturing the mouse in the same window? 
}}}

What is happening:

The veto dialog is calling wxWindowGTK::GTKReleaseMouseAndNotify(), but despite this the window (wxAuiTabCtrl) is still in the mouse capture stack.  This is because DoReleaseMouse() is being called from GTKReleaseMouseAndNotify() when ReleaseMouse() should be called instead.  DoReleaseMouse() releases the mouse, but ReleaseMouse() cleans up the capture stack and also calls DoReleaseMouse().


The fix:

'''In file: src/gtk/window.cpp'''

{{{
void wxWindowGTK::GTKReleaseMouseAndNotify()
{
    DoReleaseMouse();
    wxMouseCaptureLostEvent evt(GetId());
    evt.SetEventObject( this );
    HandleWindowEvent( evt );
}
}}}

'''DoReleaseMouse() should change to ReleaseMouse()'''

{{{
void wxWindowGTK::GTKReleaseMouseAndNotify()
{
    ReleaseMouse();
    wxMouseCaptureLostEvent evt(GetId());
    evt.SetEventObject( this );
    HandleWindowEvent( evt );
}
}}}