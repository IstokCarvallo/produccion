$PBExportHeader$w_maed_muevecajas.srw
forward
global type w_maed_muevecajas from w_mant_encab_deta_csd
end type
type st_6 from statictext within w_maed_muevecajas
end type
type st_1 from statictext within w_maed_muevecajas
end type
type gb_3 from groupbox within w_maed_muevecajas
end type
type st_2 from statictext within w_maed_muevecajas
end type
type st_3 from statictext within w_maed_muevecajas
end type
type st_4 from statictext within w_maed_muevecajas
end type
type em_desde from editmask within w_maed_muevecajas
end type
type em_hasta from editmask within w_maed_muevecajas
end type
type uo_selcliente from uo_seleccion_clientesprod within w_maed_muevecajas
end type
type uo_selplantas from uo_seleccion_plantas within w_maed_muevecajas
end type
end forward

global type w_maed_muevecajas from w_mant_encab_deta_csd
integer width = 3963
integer height = 1988
string title = "CAMBIO CAJAS"
string menuname = ""
event ue_imprimir ( )
event ue_validapassword ( )
st_6 st_6
st_1 st_1
gb_3 gb_3
st_2 st_2
st_3 st_3
st_4 st_4
em_desde em_desde
em_hasta em_hasta
uo_selcliente uo_selcliente
uo_selplantas uo_selplantas
end type
global w_maed_muevecajas w_maed_muevecajas

type variables
Boolean	ib_existe_folio, ib_primera_entrada

end variables

forward prototypes
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine borra_tablas (integer ai_borra)
public subroutine borra_historico (integer ai_dato)
public subroutine wf_actualiza_estado ()
public function boolean wf_actualiza ()
end prototypes

event ue_validapassword();Str_mant		lstr_mant

lstr_mant.Argumento[1]	=	"Produccion"
lstr_mant.Argumento[2]	=	gs_clavecomext

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

If lstr_mant.Respuesta = 0 Then
	Close(This)
	Return
End If
end event

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_1.GrupoFecha	=	ldt_FechaHora
dw_2.GrupoFecha	=	ldt_FechaHora

If Not dw_2.uf_check_required(0) Then Return False
If Not dw_1.uf_validate(0) Then Return False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If Borrando Then
	If dw_1.Update(True, False) = 1 Then
		If dw_2.Update(True, False) = 1 Then
			Commit;
			
			If sqlca.SQLCode <> 0 Then
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			Else
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
	End If
Else
	If dw_2.Update(True, False) = 1 Then
		If dw_1.Update(True, False) = 1 Then
			Commit;
			
			If sqlca.SQLCode <> 0 Then
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			Else
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
				
				wf_Actualiza_Estado()
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
	End If
End If

sqlca.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
end function

public subroutine borra_tablas (integer ai_borra);
end subroutine

public subroutine borra_historico (integer ai_dato);
end subroutine

public subroutine wf_actualiza_estado ();Long		ll_numero, ll_fila, ll_numero2, ll_cajas
Boolean	lb_actualiza = False

ll_numero	=	Long(em_desde.Text)
ll_numero2	=	Long(em_hasta.Text)

FOR ll_fila = 1 TO dw_1.RowCount()
	If dw_1.Object.pafr_ccajas[ll_fila] > 0 Then
		lb_actualiza = True
	End If	
NEXT

If lb_actualiza = True Then	
	SELECT sum(pafr_ccajas)
	INTO :ll_cajas
	FROM dbo.palletfruta
	WHERE paen_numero = :ll_numero
	AND	clie_codigo = :uo_SelCliente.Codigo
	AND	plde_codigo = :uo_SelPlantas.Codigo;
	
	UPDATE dbo.palletencab 
		SET	paen_estado = 1,
				paen_ccajas = :ll_cajas
	WHERE paen_numero = :ll_numero
	AND	clie_codigo = :uo_SelCliente.Codigo
	AND	plde_codigo = :uo_SelPlantas.Codigo;
	
	If sqlca.SQLCode = -1 Then F_errorbasedatos(sqlca,"Lectura tabla Palletencab Origen")
	
	ll_cajas = 0
	
	SELECT sum(pafr_ccajas)
	INTO :ll_cajas
	FROM dbo.palletfruta
	WHERE paen_numero = :ll_numero2
	AND	clie_codigo = :uo_SelCliente.Codigo
	AND	plde_codigo = :uo_SelPlantas.Codigo;
	
	UPDATE dbo.palletencab 
		SET	paen_estado = 1,
				paen_ccajas = :ll_cajas
	WHERE paen_numero = :ll_numero2
	AND	clie_codigo = :uo_SelCliente.Codigo
	AND	plde_codigo = :uo_SelPlantas.Codigo;
	
	If sqlca.SQLCode = -1 Then F_errorbasedatos(sqlca,"Lectura tabla Palletencab Destino")
	
End If

lb_actualiza = False
	

end subroutine

public function boolean wf_actualiza ();Boolean	lb_Retorno = True
Long		ll_Folio1, ll_Folio2, ll_Caja, ll_Fila

ll_Folio1	=	Long(em_desde.Text)
ll_Folio2	=	Long(em_hasta.Text)

For ll_Fila = 1 To dw_2.RowCount()
	If dw_2.Object.Traspasa[ll_Fila] = 1 Then 
		ll_Caja	= dw_2.Object.pafr_secuen[ll_Fila]

		Update dbo.palletfruta
			set pafr_ccajas = 0
			where clie_codigo = :uo_SelCliente.Codigo
			and plde_codigo = :uo_SelPlantas.Codigo
			and paen_numero = :ll_Folio1
			and pafr_secuen = :ll_Caja;
			
		If sqlca.SQLCode = -1 Then 
			F_errorbasedatos(sqlca,"Lectura tabla Paletfruta Origen")
			lb_Retorno = False
		Else
			Update dbo.palletfruta
			set pafr_ccajas = 1
			where clie_codigo = :uo_SelCliente.Codigo
			and plde_codigo = :uo_SelPlantas.Codigo
			and paen_numero = :ll_Folio2
			and pafr_secuen = :ll_Caja;
			
			If sqlca.SQLCode = -1 Then 
				F_Errorbasedatos(sqlca,"Lectura tabla Paletfruta Destino")
				lb_Retorno = False
			End If
		End If
	End If
Next 

If lb_Retorno Then wf_Actualiza_Estado()

Return lb_Retorno
end function

event open;call super::open;Boolean lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_CodPlanta)

	istr_mant.dw				= dw_1
	istr_mant.solo_consulta = False
	
	dw_2.SetTransObject(Sqlca)
	
	ib_primera_entrada = True
	
	PostEvent("ue_validapassword")
End If
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, pallet, ll_numero, ll_numero2

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	
	ll_numero 	= Long(em_desde.Text)
	ll_numero2  = Long(em_hasta.Text)
	
	ll_fila_e	= dw_2.Retrieve(uo_SelCliente.Codigo, uo_SelPlantas.Codigo,ll_numero, ll_numero2)
	ll_fila_d	= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlantas.Codigo,ll_numero2, ll_numero)
	
	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	Else
		DO
			If ll_fila_d = -1 Then
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Else                                                    
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled	= True
				pb_imprimir.Enabled	= True
				pb_ins_det.Enabled	= True

				If ll_fila_d > 0 Then
					pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
   				Else
				End If
			End If
		LOOP WHILE respuesta = 1

		If respuesta = 2 Then Close(This)
	End If
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

If respuesta = 2 Then Close(This)
end event

on w_maed_muevecajas.create
int iCurrent
call super::create
this.st_6=create st_6
this.st_1=create st_1
this.gb_3=create gb_3
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.uo_selcliente=create uo_selcliente
this.uo_selplantas=create uo_selplantas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_6
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.gb_3
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.em_desde
this.Control[iCurrent+8]=this.em_hasta
this.Control[iCurrent+9]=this.uo_selcliente
this.Control[iCurrent+10]=this.uo_selplantas
end on

on w_maed_muevecajas.destroy
call super::destroy
destroy(this.st_6)
destroy(this.st_1)
destroy(this.gb_3)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.uo_selcliente)
destroy(this.uo_selplantas)
end on

event ue_nuevo;dw_1.Reset()
dw_2.Reset()

em_desde.Text = ''
em_hasta.Text = ''

em_desde.SetFocus()


end event

event resize;call super::resize;st_2.Width	=	This.WorkSpaceWidth() - 600

dw_2.Width	=	(st_2.Width / 2)
dw_1.Width	=	dw_2.Width

dw_2.x		=	st_2.x
dw_1.x		=	dw_2.x + dw_2.Width
dw_2.y		=	st_2.y + st_2.Height + 10
dw_1.y		=	dw_2.y
dw_1.Height	=	This.WorkSpaceHeight() - dw_1.y - 41
dw_2.Height	=	dw_1.Height


end event

event ue_guardar;If dw_1.AcceptText() = -1 Then Return

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

If Message.DoubleParm = -1 Then Return

If wf_actualiza() Then
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
Else
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	Return
End If

pb_eliminar.Enabled	= 	True
pb_buscar.Enabled	=	True
pb_nuevo.Enabled		=	True


end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_muevecajas
integer x = 1746
integer y = 348
integer width = 1545
integer height = 1456
integer taborder = 100
string title = "Pallet Destino"
string dataobject = "dw_mueve_cajaspalletfruta"
boolean hscrollbar = false
boolean livescroll = false
end type

event dw_1::doubleclicked;//
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_muevecajas
integer x = 18
integer y = 348
integer height = 1456
boolean titlebar = true
string title = "Pallet Origen"
string dataobject = "dw_mueve_cajaspalletfruta_origen"
boolean vscrollbar = true
end type

event dw_2::doubleclicked;//
end event

event dw_2::itemchanged;call super::itemchanged;String	ls_Columna

ls_Columna = dwo.Name

Choose Case ls_Columna
	Case 'traspasa'
		IF Integer(Data) = 1 Then
			dw_1.Object.pafr_ccajas[Row] = 1 
			dw_2.Object.pafr_ccajas[Row] = 0 
		End If
End Choose 
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_muevecajas
integer x = 3392
integer y = 252
end type

event pb_nuevo::clicked;Parent.TriggerEvent("ue_nuevo")
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_muevecajas
boolean visible = false
integer x = 3392
integer y = 432
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_muevecajas
integer x = 3392
integer y = 612
end type

event pb_grabar::clicked;If dw_2.RowCount() > 0 AND dw_1.RowCount() > 0 Then
	Parent.TriggerEvent("ue_guardar")
Else
	MessageBox("Atención","No Existe Informacion para Mover Cajas.")
	Return
End If	


end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_muevecajas
boolean visible = false
integer x = 3392
integer y = 792
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_muevecajas
integer x = 3392
integer y = 972
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_muevecajas
boolean visible = false
integer x = 3392
integer y = 1276
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_muevecajas
boolean visible = false
integer x = 3392
integer y = 1512
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_muevecajas
integer x = 3401
integer y = 40
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
end type

event pb_buscar::clicked;IF em_desde.Text <> '' AND em_hasta.Text <> '' THEN
//	IF dw_2.RowCount() > 0 AND dw_1.RowCount() > 0 THEN
		Parent.TriggerEvent("ue_recuperadatos")
//	ELSE
//		MessageBox("Atención","No Existe informacion para Mover Cajas.")
//		Return
//	END IF	
ELSE
	MessageBox("Atención","Falta Número de Pallet para Recuperar Datos.")
	Return
END IF	
end event

type st_6 from statictext within w_maed_muevecajas
integer x = 87
integer y = 64
integer width = 306
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
string text = "Cliente"
boolean focusrectangle = false
end type

type st_1 from statictext within w_maed_muevecajas
integer x = 1728
integer y = 64
integer width = 306
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
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_maed_muevecajas
integer x = 128
integer y = 140
integer width = 3054
integer height = 164
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Pallet"
end type

type st_2 from statictext within w_maed_muevecajas
integer x = 37
integer y = 20
integer width = 3246
integer height = 312
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_maed_muevecajas
integer x = 393
integer y = 216
integer width = 302
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
string text = "Contiene"
boolean focusrectangle = false
end type

type st_4 from statictext within w_maed_muevecajas
integer x = 2043
integer y = 212
integer width = 311
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
string text = "Devolver"
boolean focusrectangle = false
end type

type em_desde from editmask within w_maed_muevecajas
integer x = 704
integer y = 188
integer width = 453
integer height = 100
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
maskdatatype maskdatatype = stringmask!
end type

event modified;IF This.Text <> '' THEN
	IF em_hasta.Text = This.Text  THEN
		MessageBox("Atención","Pallet Destino debe Ser Distinto a Origen.")
		This.Text = ''
		This.SetFocus()
		Return
	END IF
END IF	

end event

type em_hasta from editmask within w_maed_muevecajas
integer x = 2354
integer y = 188
integer width = 453
integer height = 100
integer taborder = 40
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
maskdatatype maskdatatype = stringmask!
end type

event modified;IF This.Text <> '' THEN
	IF em_desde.Text = This.Text  THEN
		MessageBox("Atención","Pallet Origen debe Ser Distinto a Destino.")
		This.Text = ''
		This.SetFocus()
		Return
	END IF
END IF	

end event

type uo_selcliente from uo_seleccion_clientesprod within w_maed_muevecajas
event destroy ( )
integer x = 352
integer y = 56
integer height = 84
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_maed_muevecajas
integer x = 1989
integer y = 56
integer height = 84
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

