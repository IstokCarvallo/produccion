$PBExportHeader$w_mant_mues_valofactprod.srw
$PBExportComments$Mantención Valores de Facturación por Productor.
forward
global type w_mant_mues_valofactprod from w_mant_tabla
end type
type st_2 from statictext within w_mant_mues_valofactprod
end type
type st_3 from statictext within w_mant_mues_valofactprod
end type
type em_produc from editmask within w_mant_mues_valofactprod
end type
type cb_2 from uo_buscar within w_mant_mues_valofactprod
end type
type sle_nompro from singlelineedit within w_mant_mues_valofactprod
end type
type sle_nomzona from singlelineedit within w_mant_mues_valofactprod
end type
type st_5 from statictext within w_mant_mues_valofactprod
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_valofactprod
end type
type cb_copia from commandbutton within w_mant_mues_valofactprod
end type
type str_anexos from structure within w_mant_mues_valofactprod
end type
end forward

type str_anexos from structure
	string		titulo
	string		nominf
	string		nomdw
end type

global type w_mant_mues_valofactprod from w_mant_tabla
integer width = 3785
integer height = 1936
string title = "VALORES DE FACTURACION POR PRODUCTOR"
st_2 st_2
st_3 st_3
em_produc em_produc
cb_2 cb_2
sle_nompro sle_nompro
sle_nomzona sle_nomzona
st_5 st_5
uo_selcliente uo_selcliente
cb_copia cb_copia
end type
global w_mant_mues_valofactprod w_mant_mues_valofactprod

type variables
w_mant_deta_valofactprod	iw_mantencion
DataWindowChild				idwc_especie
end variables

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

istr_Mant.Argumento[1] = String(uo_SelCliente.Codigo)

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

istr_info.titulo	= "Valores de Facturación por Productor"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_valofactprod"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,Long(istr_mant.argumento[2]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta
DO
	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo, Long(istr_mant.argumento[2]))
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN
	Close(This)
ELSE
	pb_insertar.Enabled	= True
END IF
end event

on w_mant_mues_valofactprod.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_3=create st_3
this.em_produc=create em_produc
this.cb_2=create cb_2
this.sle_nompro=create sle_nompro
this.sle_nomzona=create sle_nomzona
this.st_5=create st_5
this.uo_selcliente=create uo_selcliente
this.cb_copia=create cb_copia
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.em_produc
this.Control[iCurrent+4]=this.cb_2
this.Control[iCurrent+5]=this.sle_nompro
this.Control[iCurrent+6]=this.sle_nomzona
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.uo_selcliente
this.Control[iCurrent+9]=this.cb_copia
end on

on w_mant_mues_valofactprod.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_3)
destroy(this.em_produc)
destroy(this.cb_2)
destroy(this.sle_nompro)
destroy(this.sle_nomzona)
destroy(this.st_5)
destroy(this.uo_selcliente)
destroy(this.cb_copia)
end on

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

istr_Mant.Argumento[1] = String(uo_SelCliente.Codigo)

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

event open;call super::open;Boolean lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	
	dw_1.GetChild("espe_codigo", idwc_especie)
	idwc_especie.SetTransObject(sqlca)
	idwc_especie.Retrieve()
	
	buscar	=	"Código Especie:Nespe_codigo,Código Variedad:Nvari_codigo,Nombre Variedad:Svari_nombre,Calibre:Svaca_calibr"
	ordenar	=	"Código Especie:espe_codigo,Código Variedad:vari_codigo,Nombre Variedad:vari_nombre,Calibre:vaca_calibr"
End If
end event

event ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	istr_Mant.Argumento[1] = String(uo_SelCliente.Codigo)
	
	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_fila, ll_productor
Integer	li_secuencia

ll_productor	=	Long(em_produc.Text)

SELECT	Max(vafa_secuen)
	INTO	:li_secuencia
	FROM	dbo.valofactprod
	WHERE	clie_codigo	=	:uo_SelCliente.Codigo
	AND   prod_codigo =  :ll_Productor;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Valores de Facturación por Productor")
	
	Message.DoubleParm	=	-1
ELSEIF sqlca.SQLCode = 100 OR IsNull(li_secuencia) THEN
	li_secuencia	=	0
END IF

FOR ll_fila	= 1 to dw_1.RowCount()
	IF IsNull(dw_1.Object.vafa_secuen[ll_fila]) OR dw_1.Object.vafa_secuen[ll_fila] = 0 THEN
		li_secuencia ++
		dw_1.Object.vafa_secuen[ll_fila]	= li_secuencia
	END IF
NEXT
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_valofactprod
integer y = 584
integer width = 3141
integer height = 1216
integer taborder = 50
string dataobject = "dw_mues_valofactprod"
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_valofactprod
integer width = 3113
integer height = 464
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_valofactprod
integer x = 3392
integer y = 108
integer taborder = 40
boolean enabled = false
end type

event pb_lectura::clicked;call super::clicked;uo_SelCliente.Bloquear(True)
em_produc.Enabled		=	False
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_valofactprod
integer x = 3392
integer y = 564
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;uo_SelCliente.Bloquear(False)
em_produc.Enabled			=	True	

pb_lectura.Enabled		= 	False

em_produc.Text			=	''
sle_nompro.Text			=	''
sle_nomzona.Text			=	''

em_produc.SetFocus()
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_valofactprod
integer x = 3392
integer y = 740
integer taborder = 70
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_valofactprod
integer x = 3392
integer y = 916
integer taborder = 80
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_valofactprod
integer x = 3392
integer y = 1092
integer taborder = 90
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_valofactprod
integer x = 3392
integer y = 1268
integer taborder = 100
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_valofactprod
integer x = 3392
integer y = 1572
integer taborder = 110
end type

type st_2 from statictext within w_mant_mues_valofactprod
integer x = 183
integer y = 368
integer width = 293
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Zona"
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_mues_valofactprod
integer x = 187
integer y = 252
integer width = 297
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type em_produc from editmask within w_mant_mues_valofactprod
event modified pbm_enmodified
integer x = 567
integer y = 248
integer width = 219
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
string displaydata = ""
end type

event modified;Integer	li_zona
String	ls_productor, ls_zona
Long		ll_codigo


IF This.Text <> '' THEN
	ll_codigo = Long(This.Text)
	
	SELECT	pro.zona_codigo, pro.prod_nombre, zon.zona_nombre
		INTO 	:li_zona, :ls_productor, :ls_zona
		FROM	dbo.PRODUCTORES as pro, dbo.ZONAS as zon
		WHERE	pro.prod_codigo = :ll_codigo
		AND	zon.zona_codigo = pro.zona_codigo;
	
	IF sqlca.SQLCode = -1 THEN
		MessageBox("Error","Error al intentar conección a Base de Datos",Information!, Ok!)
		sle_nompro.text	= ""
		sle_nomzona.text	= ""
		This.SetFocus()
		RETURN
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Error","Código de Productor no ha sido ingresado.",Information!, Ok!)
		sle_nompro.text	= ""
		sle_nomzona.text	= ""
		This.SetFocus()
		RETURN
	ELSE
		istr_mant.argumento[2]	= String(ll_codigo)
		istr_mant.argumento[3]	= ls_productor
		sle_nompro.text			= ls_productor
		sle_nomzona.text			= ls_zona
		pb_lectura.Enabled		= True
		pb_lectura.SetFocus()
	END IF
END IF	
end event

type cb_2 from uo_buscar within w_mant_mues_valofactprod
event clicked pbm_bnclicked
integer x = 805
integer y = 248
integer width = 96
integer height = 84
integer taborder = 0
boolean bringtotop = true
end type

event clicked;istr_busq.argum[1]  = ''

OpenWithParm(w_busc_productores, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[1] = "" THEN
	em_produc.SetFocus()
ELSE
	em_produc.Text			= istr_busq.argum[1]
	sle_nompro.Text			= istr_busq.argum[2]
	sle_nomzona.Text			= istr_busq.argum[4]

	istr_mant.argumento[2]	= istr_busq.argum[1]
	istr_mant.argumento[3]	= istr_busq.argum[2]

	pb_lectura.Enabled		= True
	pb_lectura.SetFocus()
END IF
end event

type sle_nompro from singlelineedit within w_mant_mues_valofactprod
integer x = 919
integer y = 248
integer width = 1627
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_nomzona from singlelineedit within w_mant_mues_valofactprod
integer x = 567
integer y = 364
integer width = 1979
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_mant_mues_valofactprod
integer x = 187
integer y = 144
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_valofactprod
event destroy ( )
integer x = 567
integer y = 128
integer height = 100
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type cb_copia from commandbutton within w_mant_mues_valofactprod
integer x = 2597
integer y = 236
integer width = 576
integer height = 112
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Copiar"
end type

event clicked;Integer		li_Respuesta
String			ls_Mensaje
Str_mant		lstr_mant

istr_mant.argumento[1]	=	String(uo_SelCliente.Codigo)

If UpperBound(istr_mant.argumento) >= 2 Then 
	If IsNull(istr_mant.argumento[2]) Or  istr_mant.argumento[2] = '' Then 
		MessageBox('Atención', 'Debe seleccionar un productor')
	Else
		li_Respuesta = MessageBox('Atención', 'Se copiara Matriz de precios del Productor a Seleccionar.' + &
					'~nSi existen datos del productor actual (' + istr_mant.argumento[2] + ').' + &
					'~nSeran eliminados. Desea Continuar?' , Question!, YesNo!, 2)
					
		If li_Respuesta = 1 Then
			OpenWithParm(w_copia_valoresproductor, istr_Mant)
			
			lstr_Mant = Message.PowerObjectParm
			
			If lstr_Mant.Respuesta <> 1 Then
				ls_Mensaje = 'Proceso Abortado por el Usuario...'
			Else
				ls_Mensaje = 'Preceso Terminado...'
			End If
			MessageBox('Atencion', ls_Mensaje, Exclamation!, OK!)
			Parent.TriggerEvent('ue_recuperadatos')
		Else
			Parent.TriggerEvent('ue_recuperadatos')
		End If
	End If
Else
	MessageBox('Atención', 'Debe seleccionar un productor')
End IF
end event

