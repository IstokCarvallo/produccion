$PBExportHeader$w_consulta_embarques.srw
forward
global type w_consulta_embarques from w_para_informes
end type
type dw_1 from uo_dw within w_consulta_embarques
end type
type dw_2 from uo_dw within w_consulta_embarques
end type
type em_fecha from editmask within w_consulta_embarques
end type
type st_2 from statictext within w_consulta_embarques
end type
type st_3 from statictext within w_consulta_embarques
end type
type st_4 from statictext within w_consulta_embarques
end type
type st_6 from statictext within w_consulta_embarques
end type
type st_7 from statictext within w_consulta_embarques
end type
type st_8 from statictext within w_consulta_embarques
end type
type cbx_fecha from checkbox within w_consulta_embarques
end type
type st_5 from statictext within w_consulta_embarques
end type
type pb_carga from picturebutton within w_consulta_embarques
end type
type uo_selproductor from uo_seleccion_productor_mod within w_consulta_embarques
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_consulta_embarques
end type
type uo_selnaves from uo_seleccion_naves_mod within w_consulta_embarques
end type
type uo_seltransporte from uo_seleccion_tipotransporte_mod within w_consulta_embarques
end type
type uo_selrecibidor from uo_seleccion_recibidor_mod within w_consulta_embarques
end type
type st_9 from statictext within w_consulta_embarques
end type
type st_10 from statictext within w_consulta_embarques
end type
type st_11 from statictext within w_consulta_embarques
end type
type uo_selcalibre from uo_seleccion_variecalibre_mod within w_consulta_embarques
end type
type st_12 from statictext within w_consulta_embarques
end type
type em_conten from editmask within w_consulta_embarques
end type
type cbx_conten from checkbox within w_consulta_embarques
end type
end forward

global type w_consulta_embarques from w_para_informes
integer x = 14
integer y = 32
integer width = 3886
integer height = 2280
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "AppIcon!"
event ue_recuperadatos ( )
event ue_asignacion ( )
dw_1 dw_1
dw_2 dw_2
em_fecha em_fecha
st_2 st_2
st_3 st_3
st_4 st_4
st_6 st_6
st_7 st_7
st_8 st_8
cbx_fecha cbx_fecha
st_5 st_5
pb_carga pb_carga
uo_selproductor uo_selproductor
uo_selvariedad uo_selvariedad
uo_selnaves uo_selnaves
uo_seltransporte uo_seltransporte
uo_selrecibidor uo_selrecibidor
st_9 st_9
st_10 st_10
st_11 st_11
uo_selcalibre uo_selcalibre
st_12 st_12
em_conten em_conten
cbx_conten cbx_conten
end type
global w_consulta_embarques w_consulta_embarques

type variables
str_mant 		istr_mant
Long				il_Fila
uo_Reclamos	iuo_Reclamos
end variables

event ue_recuperadatos();Long 		ll_Fila, Respuesta
Integer	li_Tipo
String		ls_Pallet, ls_Contenedor
Date		ld_Fecha

If cbx_conten.Checked Then
	ls_Contenedor	= '*'
Else
	ls_Contenedor	= em_conten.Text
End If

If cbx_Fecha.Checked Then
	ld_Fecha = Date('2100-01-01')
Else
	ld_Fecha = Date(em_Fecha.Text)
End If

/**********  Información Real  ***********/
li_Tipo = 1
/**********  Todos Los Pallet  ***********/
ls_Pallet	= '*'

DO
	ll_fila	= dw_1.Retrieve(ls_Pallet, uo_SelRecibidor.Codigo, ld_Fecha, uo_SelTransporte.Codigo, uo_SelProductor.Codigo, uo_SelNaves.Codigo, &
 										Integer(istr_Mant.Argumento[5]), uo_SelVariedad.Codigo, li_Tipo, uo_SelCalibre.Codigo, ls_Contenedor)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_carga.Enabled	= True
	ELSE
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_asignacion();graphicobject lg_Object
datawindow lg_DW
lg_Object = GetFocus()

Choose Case TypeOf (lg_Object)
	Case DataWindow!
	lg_DW = lg_Object
	ClipBoard (lg_DW.Describe ("datawindow.selected.data"))
End Choose
end event

on w_consulta_embarques.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.dw_2=create dw_2
this.em_fecha=create em_fecha
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.st_6=create st_6
this.st_7=create st_7
this.st_8=create st_8
this.cbx_fecha=create cbx_fecha
this.st_5=create st_5
this.pb_carga=create pb_carga
this.uo_selproductor=create uo_selproductor
this.uo_selvariedad=create uo_selvariedad
this.uo_selnaves=create uo_selnaves
this.uo_seltransporte=create uo_seltransporte
this.uo_selrecibidor=create uo_selrecibidor
this.st_9=create st_9
this.st_10=create st_10
this.st_11=create st_11
this.uo_selcalibre=create uo_selcalibre
this.st_12=create st_12
this.em_conten=create em_conten
this.cbx_conten=create cbx_conten
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.dw_2
this.Control[iCurrent+3]=this.em_fecha
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.st_6
this.Control[iCurrent+8]=this.st_7
this.Control[iCurrent+9]=this.st_8
this.Control[iCurrent+10]=this.cbx_fecha
this.Control[iCurrent+11]=this.st_5
this.Control[iCurrent+12]=this.pb_carga
this.Control[iCurrent+13]=this.uo_selproductor
this.Control[iCurrent+14]=this.uo_selvariedad
this.Control[iCurrent+15]=this.uo_selnaves
this.Control[iCurrent+16]=this.uo_seltransporte
this.Control[iCurrent+17]=this.uo_selrecibidor
this.Control[iCurrent+18]=this.st_9
this.Control[iCurrent+19]=this.st_10
this.Control[iCurrent+20]=this.st_11
this.Control[iCurrent+21]=this.uo_selcalibre
this.Control[iCurrent+22]=this.st_12
this.Control[iCurrent+23]=this.em_conten
this.Control[iCurrent+24]=this.cbx_conten
end on

on w_consulta_embarques.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.em_fecha)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.cbx_fecha)
destroy(this.st_5)
destroy(this.pb_carga)
destroy(this.uo_selproductor)
destroy(this.uo_selvariedad)
destroy(this.uo_selnaves)
destroy(this.uo_seltransporte)
destroy(this.uo_selrecibidor)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.st_11)
destroy(this.uo_selcalibre)
destroy(this.st_12)
destroy(this.em_conten)
destroy(this.cbx_conten)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelRecibidor.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelProductor.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelTransporte.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelNaves.Codigo) 		Then lb_Cerrar	=	True
If IsNull(uo_SelVariedad.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelCalibre.Codigo)		Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	istr_mant	= Message.PowerObjectParm
	
	If IsNull(istr_Mant.Argumento[1]) Or istr_Mant.Argumento[1] = '' Then istr_Mant.Argumento[1] = '-1'
	If IsNull(istr_Mant.Argumento[3]) Or istr_Mant.Argumento[3] = '' Then istr_Mant.Argumento[3] = '*'
	If IsNull(istr_Mant.Argumento[4]) Or istr_Mant.Argumento[4] = '' Then istr_Mant.Argumento[4] = '-1'
	If IsNull(istr_Mant.Argumento[5]) Or istr_Mant.Argumento[5] = '' Then istr_Mant.Argumento[5] = '-1'
	
	uo_SelRecibidor.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelTransporte.Seleccion(True, False)
	uo_SelNaves.Seleccion(True, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelCalibre.Seleccion(True, False)
	iuo_Reclamos	=	Create uo_Reclamos
	
	If Long(istr_Mant.Argumento[1]) <> -1 Then
		uo_SelRecibidor.cbx_Todos.Checked = False
		uo_SelRecibidor.Codigo = Long(istr_Mant.Argumento[1])
		uo_SelRecibidor.dw_Seleccion.Object.Codigo[1] = Long(istr_Mant.Argumento[1])
	End If
	
	If IsNull(istr_Mant.Argumento[2]) Or istr_Mant.Argumento[2] = '' Then 
		cbx_Fecha.Checked	= True
		em_Fecha.Enabled	= False
		em_Fecha.Text 		= '01/01/2100'
	Else
		cbx_Fecha.Checked	= False
		em_Fecha.Enabled	= True
		em_Fecha.Text 		= istr_Mant.Argumento[2]
	End If
	
	If istr_Mant.Argumento[3] <> '*' Then
		uo_SelTransporte.cbx_Todos.Checked = False
		uo_SelTransporte.Codigo = istr_Mant.Argumento[3]
		uo_SelTransporte.dw_Seleccion.Object.Codigo[1] = istr_Mant.Argumento[3]
		uo_SelNaves.Filtra(istr_Mant.Argumento[3])
	End If

	If Long(istr_Mant.Argumento[4]) <> -1 Then
		uo_SelNaves.cbx_Todos.Checked = False
		uo_SelNaves.Codigo = Long(istr_Mant.Argumento[4])
		uo_SelNaves.dw_Seleccion.Object.Codigo[1] = Long(istr_Mant.Argumento[4])
	End If
	
	uo_SelVariedad.Filtra(Long(istr_Mant.Argumento[5]))
	uo_SelCalibre.Filtra(Long(istr_Mant.Argumento[5]), -1)
	
	dw_1.SetTransObject(Sqlca)
	dw_2.SetTransObject(Sqlca)
	istr_mant.dw.ShareData(dw_2)
	
	TriggerEvent('ue_recuperadatos')
End If
end event

type pb_excel from w_para_informes`pb_excel within w_consulta_embarques
integer x = 3575
integer y = 1068
end type

type st_computador from w_para_informes`st_computador within w_consulta_embarques
integer x = 2953
integer y = 160
end type

type st_usuario from w_para_informes`st_usuario within w_consulta_embarques
integer x = 2953
integer y = 92
end type

type st_temporada from w_para_informes`st_temporada within w_consulta_embarques
integer x = 2953
integer y = 20
end type

type p_logo from w_para_informes`p_logo within w_consulta_embarques
end type

type st_titulo from w_para_informes`st_titulo within w_consulta_embarques
integer x = 293
integer y = 288
integer width = 1563
integer height = 716
borderstyle borderstyle = styleraised!
end type

type pb_acepta from w_para_informes`pb_acepta within w_consulta_embarques
string tag = "Recupera Informacion"
integer x = 3570
integer y = 356
integer taborder = 200
string picturename = "\Desarrollo 12\Imagenes\Botones\RescatarEnable.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\RescatarDisable.png"
string powertiptext = "Recupera Informacion"
end type

event pb_acepta::clicked;Parent.TriggerEvent('ue_recuperadatos')
end event

type pb_salir from w_para_informes`pb_salir within w_consulta_embarques
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3570
integer y = 748
integer taborder = 210
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

event pb_salir::clicked;CloseWithReturn(Parent, istr_mant)
end event

type dw_1 from uo_dw within w_consulta_embarques
integer x = 293
integer y = 1072
integer width = 3131
integer height = 1088
integer taborder = 11
boolean bringtotop = true
boolean titlebar = true
string title = "Consulta de Embarques"
string dataobject = "dw_consulta_embarques"
boolean hscrollbar = true
end type

event clicked;call super::clicked;String  ls_Tecla

IF KeyDown(KeyShift!) THEN
	ls_tecla = "Shift"
ELSEIF KeyDown(KeyControl!) THEN
	ls_tecla = "Control"
END IF

If Row > 0 Then
	il_fila = Row
	F_Selecciona(This, ls_tecla, Row)
Else
	/*
	Para que funcione este ordenamiento los títulos deben tener el nombre
	de la columna y terminacion "_t", de lo contrario no funcionará
	*/
	String	ls_old_sort, ls_column, ls_color_old
	Char		lc_sort
	
	IF IsNull(dwo) THEN RETURN
	
	If Right(dwo.Name,2) = '_t' Then
		ls_column	= Left (dwo.Name, Len(String(dwo.Name)) - 2)
		ls_old_sort	= This.Describe("Datawindow.Table.sort")
		ls_color_old	=This.Describe(ls_Column + "_t.Color")
	
		If ls_column = Left(ls_old_sort, Len(ls_old_sort) - 2) Then
			lc_sort = Right(ls_old_sort, 1)
			If lc_sort = 'A' Then
				lc_sort = 'D'
			Else
				lc_sort = 'A'
			End If
			This.SetSort(ls_column+" "+lc_sort)
		Else
			This.SetSort(ls_column+" A")
			This.Modify(Left(ls_old_sort, Len(ls_old_sort) - 2) + "_t.Color = " + ls_color_old)
		End If
		
		This.Modify(dwo.Name + ".Color = " + String(Rgb(0, 0, 255)))
		
		This.Sort()
	End If
End If


Return 0
end event

event rbuttondown;call super::rbuttondown;GraphicObject 		lg_Object
DataWindow 		lg_DW
m_consulta_edi		l_Menu 

IF Row =	0	THEN
	Return
ELSE
	l_Menu = CREATE m_consulta_edi
	lg_Object = GetFocus()
	Choose Case TypeOf (lg_Object)
		Case DataWindow!
			lg_DW = lg_Object
			l_Menu.m_edicion.m_Copiar.Enabled = (lg_DW.Describe ("datawindow.selected.data") <> '')
			l_Menu.m_edicion.PopMenu(w_main.PointerX(),w_main.PointerY())	
	End Choose	
End If
end event

type dw_2 from uo_dw within w_consulta_embarques
boolean visible = false
integer y = 1652
integer width = 256
integer height = 220
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mant_mues_ctlcalplanillareclamoslote"
boolean vscrollbar = false
end type

type em_fecha from editmask within w_consulta_embarques
integer x = 2345
integer y = 732
integer width = 494
integer height = 84
integer taborder = 220
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_2 from statictext within w_consulta_embarques
integer x = 3195
integer y = 304
integer width = 201
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_consulta_embarques
integer x = 1952
integer y = 740
integer width = 361
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Zarpe"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_4 from statictext within w_consulta_embarques
integer x = 347
integer y = 404
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_consulta_embarques
integer x = 347
integer y = 628
integer width = 352
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Calibre"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_consulta_embarques
integer x = 1952
integer y = 628
integer width = 361
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nave"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_8 from statictext within w_consulta_embarques
integer x = 1952
integer y = 512
integer width = 361
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo Transp"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_fecha from checkbox within w_consulta_embarques
integer x = 3269
integer y = 732
integer width = 123
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean checked = true
end type

event clicked;If This.Checked Then
	em_fecha.Text		=	''
	em_fecha.Enabled	=	False
Else
	em_fecha.Text		=	''
	em_fecha.Enabled	=	True	
	em_fecha.SetFocus()
End If
end event

type st_5 from statictext within w_consulta_embarques
integer x = 1851
integer y = 288
integer width = 1563
integer height = 716
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_carga from picturebutton within w_consulta_embarques
string tag = "Carga Informacion Seleccionada"
integer x = 3570
integer y = 552
integer width = 233
integer height = 196
integer taborder = 220
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 12\Imagenes\Botones\AceptarEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\AceptarDisab.png"
alignment htextalign = right!
string powertiptext = "Carga Informacion Seleccionada"
end type

event clicked;Long 	ll_Fila, ll_New, ll_Busca
String	ls_Busca

For ll_Fila = 1 To dw_1.RowCount()
	If dw_1.IsSelected(ll_Fila) Then
		ls_Busca = 'paen_numero = "' + dw_1.Object.dece_pallet[ll_Fila] + '" And vari_codigo = ' + String(dw_1.Object.vari_codigo[ll_Fila]) + &
				' And zona_codigo = ' + String(dw_1.Object.zona_codigo[ll_Fila]) + ' And pafr_calibr = "' + dw_1.Object.dece_calibr[ll_Fila] + &
				'" And emba_codigo = "' + dw_1.Object.emba_codigo[ll_Fila] + '" And prod_codigo = ' + String(dw_1.Object.prod_codigo[ll_Fila]) +&
				' And String(emba_fecemb, "dd/mm/yyyy") = "' + String(dw_1.Object.pafr_fecemb[ll_Fila], 'dd/mm/yyyy') + '"'
		
		ll_busca	=	dw_2.Find(ls_Busca, 1,dw_2.RowCount())
		If ll_Busca = 0 Then
			If Not iuo_Reclamos.Existe(gi_CodExport, Integer(istr_mant.Argumento[5]), dw_1.Object.vari_codigo[ll_Fila], &
							dw_1.Object.dece_pallet[ll_Fila], dw_1.Object.zona_codigo[ll_Fila], dw_1.Object.prod_codigo[ll_Fila], &
							dw_1.Object.dece_calibr[ll_Fila],  dw_1.Object.emba_codigo[ll_Fila], dw_1.Object.pafr_fecemb[ll_Fila], False, Sqlca) Then
				ll_New	= dw_2.InsertRow(0)
				
				dw_2.Object.paen_numero[ll_New]	=	dw_1.Object.dece_pallet[ll_Fila]
				dw_2.Object.vari_codigo[ll_New]		=	dw_1.Object.vari_codigo[ll_Fila]
				dw_2.Object.zona_codigo[ll_New]		=	dw_1.Object.zona_codigo[ll_Fila]
				dw_2.Object.prod_codigo[ll_New]		=	dw_1.Object.prod_codigo[ll_Fila]
				dw_2.Object.pafr_calibr[ll_New]		=	dw_1.Object.dece_calibr[ll_Fila]
				dw_2.Object.emba_codigo[ll_New]		=	dw_1.Object.emba_codigo[ll_Fila]
				dw_2.Object.emba_fecemb[ll_New]	=	dw_1.Object.pafr_fecemb[ll_Fila]
				dw_2.Object.reld_ccajas[ll_New]		=	dw_1.Object.dece_ccajas[ll_Fila]
				istr_mant.Argumento[6]					=	String(dw_1.Object.reci_codigo[ll_Fila])
				istr_mant.Argumento[7]					=	String(dw_1.Object.nave_tipotr[ll_Fila])
				istr_mant.Argumento[8]					=	String(dw_1.Object.nave_codigo[ll_Fila])
				istr_mant.Argumento[9]					=	String(dw_1.Object.puer_codigo[ll_Fila])
				istr_mant.Argumento[10]				=	String(dw_1.Object.embq_fzarpe[ll_Fila], 'dd/mm/yyyy')
				
			Else
				MessageBox('Alerta', 'El Nro. de Pallet: ' + Trim(dw_1.Object.dece_pallet[ll_Fila]) + '~r~nA sido digitado en el Reclamo Nro: ' + String(iuo_Reclamos.Numero), StopSign!, Ok!)
			End If
		End If
	End If	
Next

CloseWithReturn(Parent, istr_mant)
end event

type uo_selproductor from uo_seleccion_productor_mod within w_consulta_embarques
integer x = 763
integer y = 368
integer width = 1015
integer height = 124
integer taborder = 230
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

type uo_selvariedad from uo_seleccion_variedad_mod within w_consulta_embarques
event destroy ( )
integer x = 768
integer y = 484
integer width = 1047
integer height = 124
integer taborder = 230
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelCalibre.Filtra(Long(istr_Mant.Argumento[5]), -1)
		
	Case Else
		uo_SelCalibre.Filtra(Long(istr_Mant.Argumento[5]), This.Codigo)
		
End Choose
end event

type uo_selnaves from uo_seleccion_naves_mod within w_consulta_embarques
event destroy ( )
integer x = 2345
integer y = 600
integer width = 1047
integer height = 124
integer taborder = 210
boolean bringtotop = true
end type

on uo_selnaves.destroy
call uo_seleccion_naves_mod::destroy
end on

type uo_seltransporte from uo_seleccion_tipotransporte_mod within w_consulta_embarques
event destroy ( )
integer x = 2345
integer y = 484
integer width = 1047
integer taborder = 240
boolean bringtotop = true
end type

on uo_seltransporte.destroy
call uo_seleccion_tipotransporte_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case '*', '**'
		
	Case Else
		uo_SelNaves.Filtra(This.Codigo)
		
End Choose
end event

type uo_selrecibidor from uo_seleccion_recibidor_mod within w_consulta_embarques
integer x = 2345
integer y = 368
integer width = 1015
integer height = 124
integer taborder = 250
boolean bringtotop = true
end type

on uo_selrecibidor.destroy
call uo_seleccion_recibidor_mod::destroy
end on

type st_9 from statictext within w_consulta_embarques
integer x = 1952
integer y = 404
integer width = 361
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Recibidor"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_10 from statictext within w_consulta_embarques
integer x = 1614
integer y = 304
integer width = 201
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_11 from statictext within w_consulta_embarques
integer x = 347
integer y = 512
integer width = 352
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selcalibre from uo_seleccion_variecalibre_mod within w_consulta_embarques
integer x = 768
integer y = 600
integer width = 1047
integer taborder = 230
boolean bringtotop = true
end type

on uo_selcalibre.destroy
call uo_seleccion_variecalibre_mod::destroy
end on

type st_12 from statictext within w_consulta_embarques
integer x = 1952
integer y = 860
integer width = 361
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Contenedor"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_conten from editmask within w_consulta_embarques
integer x = 2345
integer y = 848
integer width = 878
integer height = 92
integer taborder = 220
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string minmax = "~~12"
end type

type cbx_conten from checkbox within w_consulta_embarques
integer x = 3269
integer y = 852
integer width = 123
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean checked = true
end type

event clicked;If This.Checked Then
	em_conten.Text		= ''
	em_conten.Enabled	= False
Else
	em_conten.Text		= ''
	em_conten.Enabled	= True
	em_conten.SetFocus()
End If
end event

