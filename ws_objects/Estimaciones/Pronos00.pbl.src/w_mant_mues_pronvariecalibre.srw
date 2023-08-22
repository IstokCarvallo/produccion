$PBExportHeader$w_mant_mues_pronvariecalibre.srw
forward
global type w_mant_mues_pronvariecalibre from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_pronvariecalibre
end type
type uo_selespecies from uo_seleccion_especie within w_mant_mues_pronvariecalibre
end type
type st_2 from statictext within w_mant_mues_pronvariecalibre
end type
type uo_selvariedad from uo_seleccion_variedad within w_mant_mues_pronvariecalibre
end type
type dw_2 from uo_dw within w_mant_mues_pronvariecalibre
end type
end forward

global type w_mant_mues_pronvariecalibre from w_mant_tabla
integer width = 2354
string title = "PRONOSTICO - CALIBRES POR VARIEDAD"
st_1 st_1
uo_selespecies uo_selespecies
st_2 st_2
uo_selvariedad uo_selvariedad
dw_2 dw_2
end type
global w_mant_mues_pronvariecalibre w_mant_mues_pronvariecalibre

type variables
w_mant_deta_pronvariecalibre  iw_mantencion
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
end prototypes

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_1.GrupoFecha	=	ldt_FechaHora

IF Not dw_1.uf_check_required(0) THEN RETURN False
IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If dw_1.Update(True, False) = 1 Then
	If dw_2.Update(True, False) = 1 Then
		Commit;
		
		If sqlca.SQLCode <> 0 Then
			F_ErrorBaseDatos(sqlca, This.Title)
			lb_Retorno	=	False
		Else
			lb_Retorno	=	True
				
			dw_1.ResetUpdate()
			dw_2.ResetUpdate()
		End If
	Else
		RollBack;
		IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	End If
Else
	RollBack;
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	lb_Retorno	=	False
End If

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

istr_mant.Argumento[1] = String(uo_SelEspecies.Codigo)
istr_mant.Argumento[2] = String(uo_SelVariedad.Codigo)

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "CALIBRES POR VARIEDAD PARA PRONOSTICO"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_pronvariecalibre"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelEspecies.Codigo, uo_SelVariedad.Codigo)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)


end event

event ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(uo_SelEspecies.Codigo, uo_SelVariedad.Codigo)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_nuevo.Enabled		= True
				
		IF istr_mant.Solo_Consulta <> True THEN
			pb_insertar.Enabled	= True
			pb_eliminar.Enabled	= True
			pb_grabar.Enabled	= True
		END IF	
		pb_imprimir.Enabled	= True
	ELSE
		IF istr_mant.Solo_Consulta <> True THEN	
			pb_insertar.Enabled	= True
			pb_nuevo.Enabled		= True
			pb_insertar.SetFocus()
		END IF	
		pb_imprimir.Enabled	= True
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_mant_mues_pronvariecalibre.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selespecies=create uo_selespecies
this.st_2=create st_2
this.uo_selvariedad=create uo_selvariedad
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selespecies
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.uo_selvariedad
this.Control[iCurrent+5]=this.dw_2
end on

on w_mant_mues_pronvariecalibre.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selespecies)
destroy(this.st_2)
destroy(this.uo_selvariedad)
destroy(this.dw_2)
end on

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.Borra		= True
istr_mant.Agrega	= False
istr_mant.Argumento[1] = String(uo_SelEspecies.Codigo)

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
end event

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelEspecies.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelVariedad.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelEspecies.Seleccion(False, False)
	uo_SelVariedad.Seleccion(False, False)
	uo_SelEspecies.Inicia(11)
	uo_SelVariedad.Filtra(11)
	
	dw_2.SetTransObject(SQLCA)
	istr_mant.dw2 =	dw_2
	
	buscar	= "Variedad:Nvari_codigo,Calibre:Svaca_calibr"
	ordenar	= "Variedad:vari_codigo,Calibre:vaca_calibr"	
End If
end event

event ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.Agrega	= False
	istr_mant.Borra		= False
	
	istr_mant.Argumento[1] = String(uo_SelEspecies.Codigo)
	istr_mant.Argumento[2] = String(uo_SelVariedad.Codigo)

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_pronvariecalibre
integer x = 82
integer y = 536
integer width = 1664
integer height = 1040
string dataobject = "dw_mues_pronvariecalibre"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_pronvariecalibre
integer x = 82
integer y = 100
integer width = 1664
integer height = 348
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_pronvariecalibre
integer x = 1897
integer y = 132
end type

event pb_lectura::clicked;call super::clicked;uo_SelEspecies.Bloquear(True)
uo_SelVariedad.Bloquear(True)
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_pronvariecalibre
integer x = 1897
integer y = 424
boolean enabled = false
end type

event pb_nuevo::clicked;call super::clicked;uo_SelEspecies.Bloquear(False)
uo_SelVariedad.Bloquear(False)

If gstr_apl.CodigoSistema <> 23 Then
	If gi_codexport = gi_cliebase Then istr_mant.Solo_Consulta = True
End If	

end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_pronvariecalibre
integer x = 1897
integer y = 572
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_pronvariecalibre
integer x = 1897
integer y = 800
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_pronvariecalibre
integer x = 1897
integer y = 1016
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_pronvariecalibre
integer x = 1897
integer y = 1232
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_pronvariecalibre
integer x = 1897
integer y = 1448
integer height = 228
end type

type st_1 from statictext within w_mant_mues_pronvariecalibre
integer x = 197
integer y = 184
integer width = 247
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
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type uo_selespecies from uo_seleccion_especie within w_mant_mues_pronvariecalibre
event destroy ( )
integer x = 535
integer y = 180
integer height = 84
integer taborder = 20
boolean bringtotop = true
end type

on uo_selespecies.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelVariedad.Filtra(This.Codigo)
		
End Choose
end event

type st_2 from statictext within w_mant_mues_pronvariecalibre
integer x = 197
integer y = 300
integer width = 279
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
boolean enabled = false
string text = "Variedad"
boolean focusrectangle = false
end type

type uo_selvariedad from uo_seleccion_variedad within w_mant_mues_pronvariecalibre
event destroy ( )
integer x = 535
integer y = 296
integer height = 84
integer taborder = 30
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type dw_2 from uo_dw within w_mant_mues_pronvariecalibre
boolean visible = false
integer x = 1847
integer y = 1736
integer width = 334
integer height = 244
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mant_mues_calibresdeta"
boolean vscrollbar = false
end type

