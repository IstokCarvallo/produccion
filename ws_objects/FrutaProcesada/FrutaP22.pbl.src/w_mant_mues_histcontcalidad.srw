$PBExportHeader$w_mant_mues_histcontcalidad.srw
$PBExportComments$Mantención Histórico de Control de Calidad.
forward
global type w_mant_mues_histcontcalidad from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_histcontcalidad
end type
type em_fecha from editmask within w_mant_mues_histcontcalidad
end type
type st_3 from statictext within w_mant_mues_histcontcalidad
end type
type st_4 from statictext within w_mant_mues_histcontcalidad
end type
type rb_pallet from radiobutton within w_mant_mues_histcontcalidad
end type
type rb_lote from radiobutton within w_mant_mues_histcontcalidad
end type
type ddlb_estado from dropdownlistbox within w_mant_mues_histcontcalidad
end type
type em_lote from editmask within w_mant_mues_histcontcalidad
end type
type st_lote from statictext within w_mant_mues_histcontcalidad
end type
type st_estado from statictext within w_mant_mues_histcontcalidad
end type
type dw_2 from datawindow within w_mant_mues_histcontcalidad
end type
type st_observa from statictext within w_mant_mues_histcontcalidad
end type
type em_observa from editmask within w_mant_mues_histcontcalidad
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_histcontcalidad
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_histcontcalidad
end type
end forward

global type w_mant_mues_histcontcalidad from w_mant_tabla
integer width = 3538
integer height = 1968
string title = "HISTORIA CONTROL DE CALIDAD"
event ue_validapassword ( )
st_1 st_1
em_fecha em_fecha
st_3 st_3
st_4 st_4
rb_pallet rb_pallet
rb_lote rb_lote
ddlb_estado ddlb_estado
em_lote em_lote
st_lote st_lote
st_estado st_estado
dw_2 dw_2
st_observa st_observa
em_observa em_observa
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_mant_mues_histcontcalidad w_mant_mues_histcontcalidad

type variables
Long	il_newestado
w_mant_deta_histcontcalidad	iw_mantencion
end variables

event ue_validapassword();Str_mant					lstr_mant

lstr_mant.Argumento[1]	=	"Producción"
lstr_mant.Argumento[2]	=	gs_Password
lstr_mant.Argumento[3]	=	gstr_us.Nombre

OpenWithParm(w_password, lstr_mant)


lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

event ue_nuevo;istr_mant.Borra		= False
istr_mant.Agrega	= True

istr_mant.Argumento[1] = String(uo_SelCliente.Codigo)
istr_mant.Argumento[2] = String(uo_SelPlanta.Codigo)

OpenWithParm(iw_mantencion, istr_mant)

If dw_1.RowCount() > 0 And Not pb_eliminar.Enabled Then 
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled	= True
End If

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		ll_fila, ll_lote

Str_info	lstr_info

lstr_info.titulo	=	"INFORME CONTROL DE CALIDAD"
lstr_info.copias	=	1

If rb_pallet.Checked Then
	ll_lote = -1
Else	
	ll_lote  =  Long(em_lote.Text)
End If

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_histocontcalidad"
vinf.dw_1.SetTransObject(sqlca)

ll_fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, Datetime(em_Fecha.Text), ll_lote)

If ll_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.",StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

event ue_recuperadatos;Long		ll_Fila, ll_Respuesta

w_main.SetMicroHelp("Recuperando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

Do
	If rb_pallet.Checked Then
		ll_Fila	= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Datetime(em_Fecha.Text))
		
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled	= True
		pb_insertar.Enabled	= True
		
	Else
		ll_Fila	= dw_2.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Long(em_lote.Text))
		
		If ll_Fila > 0 Then
			pb_eliminar.Enabled	= False
			pb_grabar.Enabled	= True
			pb_insertar.Enabled	= False
		End If	
	End If	
	
	If ll_Fila = -1 Then
		ll_Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ElseIf ll_Fila > 0 Then
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
	Else
		pb_insertar.SetFocus()
	End If
Loop While ll_Respuesta = 1

If ll_Respuesta = 2 Then Close(This)
end event

on w_mant_mues_histcontcalidad.create
int iCurrent
call super::create
this.st_1=create st_1
this.em_fecha=create em_fecha
this.st_3=create st_3
this.st_4=create st_4
this.rb_pallet=create rb_pallet
this.rb_lote=create rb_lote
this.ddlb_estado=create ddlb_estado
this.em_lote=create em_lote
this.st_lote=create st_lote
this.st_estado=create st_estado
this.dw_2=create dw_2
this.st_observa=create st_observa
this.em_observa=create em_observa
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.em_fecha
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.st_4
this.Control[iCurrent+5]=this.rb_pallet
this.Control[iCurrent+6]=this.rb_lote
this.Control[iCurrent+7]=this.ddlb_estado
this.Control[iCurrent+8]=this.em_lote
this.Control[iCurrent+9]=this.st_lote
this.Control[iCurrent+10]=this.st_estado
this.Control[iCurrent+11]=this.dw_2
this.Control[iCurrent+12]=this.st_observa
this.Control[iCurrent+13]=this.em_observa
this.Control[iCurrent+14]=this.uo_selcliente
this.Control[iCurrent+15]=this.uo_selplanta
end on

on w_mant_mues_histcontcalidad.destroy
call super::destroy
destroy(this.st_1)
destroy(this.em_fecha)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.rb_pallet)
destroy(this.rb_lote)
destroy(this.ddlb_estado)
destroy(this.em_lote)
destroy(this.st_lote)
destroy(this.st_estado)
destroy(this.dw_2)
destroy(this.st_observa)
destroy(this.em_observa)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

IF dw_1.GetItemStatus(il_fila, 0, Primary!) = New! OR &
	dw_1.GetItemStatus(il_fila, 0, Primary!) = NewModified! THEN
	SetPointer(HourGlass!)
	
	ib_borrar = True
	w_main.SetMicroHelp("Validando la eliminación...")
	
	Message.DoubleParm = 0
	
	This.TriggerEvent ("ue_validaborrar")
	
	IF Message.DoubleParm = -1 THEN RETURN
	
	istr_mant.borra	= True
	istr_mant.agrega	= False
	
	istr_mant.Argumento[1] = String(uo_SelCliente.Codigo)
	istr_mant.Argumento[2] = String(uo_SelPlanta.Codigo)

	OpenWithParm(iw_mantencion, istr_mant) 
	
	istr_mant = Message.PowerObjectParm
	
	IF istr_mant.respuesta = 1 THEN
		IF dw_1.DeleteRow(0) = 1 THEN
			ib_borrar = False
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_borrar = False
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
		
		IF dw_1.RowCount() = 0 THEN
			pb_eliminar.Enabled = False
		ELSE
			il_fila = dw_1.GetRow()
			dw_1.SelectRow(il_fila,True)
		END IF
	END IF
	
	istr_mant.borra	 = False
END IF
end event

event open;call super::open;Boolean lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	
	uo_SelPlanta.Filtra(1)
	
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_CodPlanta)
	
	em_fecha.Text				=	String(Today())
	
	dw_2.SetTransObject(SQLCA)
	
	buscar	=	"Número de Pallet:Npaen_numero,Estado Anterior:Ncoca_estant,Estado Nuevo:Ncoca_estneo"
	ordenar	=	"Número de Pallet:paen_numero,Estado Anterior:coca_estant,Estado Nuevo:coca_estneo"
	PostEvent("ue_validapassword")
End If
end event

event ue_modifica;call super::ue_modifica;If dw_1.RowCount() > 0 AND &
	(dw_1.GetItemStatus(il_fila, 0, Primary!) = New! Or dw_1.GetItemStatus(il_fila, 0, Primary!) = NewModified!) Then
	
	istr_mant.Agrega	= False
	istr_mant.Borra		= False
	
	istr_mant.Argumento[1] = String(uo_SelCliente.Codigo)
	istr_mant.Argumento[2] = String(uo_SelPlanta.Codigo)
	
	OpenWithParm(iw_mantencion, istr_mant)
End If
end event

event ue_listo;If rb_pallet.Checked Then

	If dw_1.RowCount() > 0 Then
		pb_Imprimir.Enabled	=	True
		
		If istr_mant.Solo_Consulta Then
			pb_Eliminar.Enabled	=	False
			pb_Grabar.Enabled	=	False
			pb_Insertar.Enabled	=	False
		Else
			pb_Eliminar.Enabled	=	True
			pb_Grabar.Enabled	=	True
			pb_Insertar.Enabled	=	True
		End If
	Else
		If istr_mant.Solo_Consulta Then
			pb_Insertar.Enabled	=	False
		Else
			pb_Insertar.Enabled	=	True
		End If
	End If
End If	

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

event ue_antesguardar;call super::ue_antesguardar;Long			ll_fila, ll_filnew, ll_new, ll_pallet
Integer		li_Cliente, li_planta, li_secuen
DateTime	ld_fecha

If rb_lote.Checked Then
	If dw_2.RowCount() > 0 Then
		For ll_fila = 1 To dw_2.RowCount()
			ll_filnew = dw_1.InsertRow(0)
			
			dw_1.Object.clie_codigo[ll_filnew] 	= dw_2.Object.clie_codigo[ll_fila]
			dw_1.Object.paen_numero[ll_filnew] = dw_2.Object.paen_numero[ll_fila]
			dw_1.Object.plde_codigo[ll_filnew] 	= dw_2.Object.plde_codigo[ll_fila]
			dw_1.Object.coca_fechac[ll_filnew] 	= Date(em_fecha.Text)
			dw_1.Object.coca_estant[ll_filnew] 	= dw_2.Object.paen_concal[ll_fila]
			dw_1.Object.coca_secuen[ll_filnew] 	= ll_fila + 1
			dw_1.Object.coca_estneo[ll_filnew] 	= il_newestado
			dw_1.Object.coca_observ[ll_filnew] 	= em_observa.Text
		Next
	End If
Else
	FOR ll_new = 1 TO dw_1.RowCount()
		li_Cliente	=	dw_1.Object.clie_codigo[ll_new]
		ll_pallet	=	dw_1.Object.paen_numero[ll_new]
		li_planta	=	dw_1.Object.plde_codigo[ll_new]
		ld_fecha	=	dw_1.Object.coca_fechac[ll_new]
		
		If li_secuen = 0 Then
			SELECT	Max(coca_secuen)
				INTO	:li_secuen
				FROM	dbo.histcontcalidad
				WHERE	clie_codigo =  :li_cliente
				AND	paen_numero	=	:ll_pallet
				AND	plde_codigo	=	:li_planta
				AND	Datediff(dd, coca_fechac, :ld_fecha) = 0;
		End If		
		
		If IsNull(li_secuen) Then li_secuen = 0
		
		If IsNull(dw_1.Object.coca_secuen[ll_new]) or dw_1.Object.coca_secuen[ll_new] = 0 Then
			li_secuen ++
			dw_1.Object.coca_secuen[ll_new] = li_secuen
		End If
	Next	
End If	
end event

event resize;call super::resize;dw_2.Height = dw_1.Height
dw_2.Width = dw_1.Width
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_histcontcalidad
integer x = 96
integer y = 492
integer width = 2898
integer height = 1240
integer taborder = 40
string dataobject = "dw_mues_histocontcalidad"
boolean hscrollbar = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_histcontcalidad
integer x = 50
integer y = 44
integer width = 2903
integer height = 428
long textcolor = 16711680
boolean enabled = true
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_histcontcalidad
integer x = 3159
integer y = 100
integer taborder = 30
end type

event pb_lectura::clicked;em_fecha.Enabled			=	False

istr_mant.Argumento[3]	=	em_fecha.Text + ' ' + String(Now(), 'hh:mm')
em_lote.Enabled 			= False
em_observa.Enabled 		= False

Parent.TriggerEvent("ue_recuperadatos")

end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_histcontcalidad
integer x = 3159
integer taborder = 50
end type

event pb_nuevo::clicked;call super::clicked;em_fecha.Enabled			=	True
em_lote.Enabled			=	True
em_observa.Enabled		=	True

em_lote.Text = ''
em_observa.Text = ''
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_histcontcalidad
integer x = 3159
integer y = 596
integer taborder = 60
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_histcontcalidad
integer x = 3159
integer y = 792
integer taborder = 70
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_histcontcalidad
integer x = 3159
integer y = 984
integer taborder = 80
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_histcontcalidad
integer x = 3159
integer y = 1176
integer taborder = 90
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_histcontcalidad
integer x = 3159
integer y = 1528
integer taborder = 100
end type

type st_1 from statictext within w_mant_mues_histcontcalidad
integer x = 151
integer y = 108
integer width = 256
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Cliente"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_histcontcalidad
integer x = 576
integer y = 352
integer width = 494
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "~t/"
boolean dropdowncalendar = true
end type

event modified;If Not f_ValidaFechaTempo(date(this.Text)) Then
	This.Text = ''
	This.SetFocus()
End If

istr_mant.Argumento[3]	=	This.Text + ' ' + String(Now(), 'hh:mm')


end event

type st_3 from statictext within w_mant_mues_histcontcalidad
integer x = 151
integer y = 364
integer width = 256
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha"
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_histcontcalidad
integer x = 151
integer y = 240
integer width = 256
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type rb_pallet from radiobutton within w_mant_mues_histcontcalidad
integer x = 1541
integer y = 120
integer width = 357
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Pallet"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	st_lote.Visible = False
	em_lote.Visible = False
	st_estado.Visible = False
	ddlb_estado.Visible = False
	dw_1.Visible = True
	dw_2.Visible = False
	em_observa.Visible = False
	st_observa.Visible = False
	
	em_lote.Text = ''
	em_observa.Text = ''
ELSE
	st_lote.Visible = True
	em_lote.Visible = True
	st_estado.Visible = True
	ddlb_estado.Visible = True	
	dw_1.Visible = False
	dw_2.Visible = True
	em_observa.Visible = True	
	st_observa.Visible = True	
	
END IF	
end event

type rb_lote from radiobutton within w_mant_mues_histcontcalidad
integer x = 1938
integer y = 120
integer width = 302
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Lote"
end type

event clicked;IF This.Checked THEN
	st_lote.Visible = True
	em_lote.Visible = True
	st_estado.Visible = True
	ddlb_estado.Visible = True
	dw_1.Visible = False
	dw_2.Visible = True
	em_observa.Visible = True
	st_observa.Visible = True
ELSE
	st_lote.Visible = False
	em_lote.Visible = False
	st_estado.Visible = False
	ddlb_estado.Visible = False
	dw_1.Visible = True
	dw_2.Visible = False
	em_observa.Visible = False
	st_observa.Visible = False
END IF	
end event

type ddlb_estado from dropdownlistbox within w_mant_mues_histcontcalidad
boolean visible = false
integer x = 2249
integer y = 236
integer width = 599
integer height = 448
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string item[] = {"Habilitado","Objetado","Rechazado"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;il_newestado = Long(Index)
end event

type em_lote from editmask within w_mant_mues_histcontcalidad
boolean visible = false
integer x = 1691
integer y = 244
integer width = 297
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
string displaydata = "@"
end type

event modified;//IF Not f_validafechatempo(date(this.Text)) THEN
//	This.Text = ''
//	This.SetFocus()
//END IF
//
//istr_mant.Argumento[3]	=	This.Text
//
//
end event

type st_lote from statictext within w_mant_mues_histcontcalidad
boolean visible = false
integer x = 1541
integer y = 252
integer width = 137
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Lote"
boolean focusrectangle = false
end type

type st_estado from statictext within w_mant_mues_histcontcalidad
boolean visible = false
integer x = 2030
integer y = 252
integer width = 206
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Estado"
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_mant_mues_histcontcalidad
boolean visible = false
integer x = 50
integer y = 488
integer width = 2898
integer height = 1240
integer taborder = 90
string title = "none"
string dataobject = "dw_mues_hiscontcalidad_lote"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_observa from statictext within w_mant_mues_histcontcalidad
boolean visible = false
integer x = 1166
integer y = 364
integer width = 283
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Obser."
boolean focusrectangle = false
end type

type em_observa from editmask within w_mant_mues_histcontcalidad
boolean visible = false
integer x = 1541
integer y = 352
integer width = 1307
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "~t/"
end type

event modified;//IF Not f_validafechatempo(date(this.Text)) THEN
//	This.Text = ''
//	This.SetFocus()
//END IF
//
//istr_mant.Argumento[3]	=	This.Text
//
//
end event

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_histcontcalidad
event destroy ( )
integer x = 576
integer y = 104
integer height = 92
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_histcontcalidad
event destroy ( )
integer x = 576
integer y = 236
integer height = 92
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

