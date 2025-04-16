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
type cb_estandar from commandbutton within w_mant_mues_valofactprod
end type
type cb_productor from commandbutton within w_mant_mues_valofactprod
end type
type cb_elimina from commandbutton within w_mant_mues_valofactprod
end type
type st_1 from statictext within w_mant_mues_valofactprod
end type
type uo_selespecie from uo_seleccion_especie within w_mant_mues_valofactprod
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
integer width = 3995
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
cb_estandar cb_estandar
cb_productor cb_productor
cb_elimina cb_elimina
st_1 st_1
uo_selespecie uo_selespecie
end type
global w_mant_mues_valofactprod w_mant_mues_valofactprod

type variables
w_mant_deta_valofactprod	iw_mantencion

uo_semanafactura	iuo_Semana
end variables

forward prototypes
public function boolean wf_validaestandar (integer cliente, integer zona, integer especie, transaction transaccion)
end prototypes

public function boolean wf_validaestandar (integer cliente, integer zona, integer especie, transaction transaccion);Boolean	lb_Retorno = True
Long		ll_Cantidad

Select		IsNull(Count(*), 0)
	Into :ll_Cantidad
	From dbo.PMGExportadora
	Where clie_codigo = :Cliente
	And zona_codigo = :Zona
	And espe_codigo = :Especie
	Using Transaccion;

If Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(Transaccion, "Lectura de Estandar Exportadora")
	lb_Retorno	=	False
ElseIf Transaccion.SQLCode = 100 Then
	lb_Retorno	=	False
Else
	If ll_Cantidad < 1 Then lb_Retorno=	False
End If
	
Return lb_Retorno
end function

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

istr_Mant.Argumento[1] = String(uo_SelCliente.Codigo)
istr_Mant.Argumento[5] = String(uo_SelEspecie.Codigo)

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
fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,Long(istr_mant.argumento[2]), uo_SelEspecie.Codigo)

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

If Not wf_ValidaEstandar(uo_SelCliente.Codigo, Integer(istr_mant.argumento[4]), uo_SelEspecie.Codigo, SQLCA) Then
	MessageBox("Atencion", "No existe estandar para zona, especie seleccionada.~r~nDebe ingresar estandar para continuar.", Exclamation!, OK!)
	Return
End If

DO
	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo, Long(istr_mant.argumento[2]), uo_SelEspecie.Codigo)
	
	If ll_fila = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ElseIf ll_fila > 0 Then
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled	= True
		cb_estandar.Enabled	= True
		cb_productor.Enabled	= True
		
		For ll_Fila = 1 To dw_1.RowCount()
			If iuo_Semana.of_Semana(dw_1.Object.vafa_fecini[ll_Fila], SQLCA) Then
				dw_1.Object.semana[ll_Fila] = iuo_Semana.Semana
			End If
		Next

	Else
		pb_insertar.SetFocus()
		pb_grabar.Enabled	= True
		cb_estandar.Enabled	= True
		cb_productor.Enabled	= True
	End If
LOOP WHILE respuesta = 1

If respuesta = 2 Then
	Close(This)
Else
	pb_insertar.Enabled	= True
End If
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
this.cb_estandar=create cb_estandar
this.cb_productor=create cb_productor
this.cb_elimina=create cb_elimina
this.st_1=create st_1
this.uo_selespecie=create uo_selespecie
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.em_produc
this.Control[iCurrent+4]=this.cb_2
this.Control[iCurrent+5]=this.sle_nompro
this.Control[iCurrent+6]=this.sle_nomzona
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.uo_selcliente
this.Control[iCurrent+9]=this.cb_estandar
this.Control[iCurrent+10]=this.cb_productor
this.Control[iCurrent+11]=this.cb_elimina
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.uo_selespecie
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
destroy(this.cb_estandar)
destroy(this.cb_productor)
destroy(this.cb_elimina)
destroy(this.st_1)
destroy(this.uo_selespecie)
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
istr_Mant.Argumento[5] = String(uo_SelEspecie.Codigo)

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
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	iuo_Semana	=	Create uo_semanafactura
	uo_SelCliente.Seleccion(False, False)
	uo_SelEspecie.Seleccion(False, False)
	
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelEspecie.Inicia(gi_CodEspecie)

	buscar	=	"Código Especie:Nespe_codigo,Código Variedad:Nvari_codigo,Nombre Variedad:Svari_nombre,Calibre:Svaca_calibr"
	ordenar	=	"Código Especie:espe_codigo,Código Variedad:vari_codigo,Nombre Variedad:vari_nombre,Calibre:vaca_calibr"
End If
end event

event ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	
	istr_Mant.Argumento[1] = String(uo_SelCliente.Codigo)
	istr_Mant.Argumento[5] = String(uo_SelEspecie.Codigo)
	
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
integer width = 3305
integer height = 1216
integer taborder = 50
string dataobject = "dw_mues_valofactprod"
end type

event dw_1::clicked;Long	ll_Fila, ll_SelectedRow

If Row = 0 Then
	/*
	Para que funcione este ordenamiento los títulos deben tener el nombre
	de la columna y terminacion "_t", de lo contrario no funcionará
	*/
	String		ls_old_sort, ls_column, ls_color_old
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

Else
	ll_SelectedRow = dw_1.GetSelectedRow(0)
	
	If KeyDown(KeyShift!) Then
		If ll_SelectedRow = 0 Then
			This.SelectRow(Row, True)
		Else
			This.SelectRow(1, False)
			If Row > ll_SelectedRow Then
				For ll_Fila = ll_SelectedRow To Row
					This.SelectRow(ll_Fila, True)
				Next
			Else
				For ll_Fila = Row To ll_SelectedRow
					This.SelectRow(ll_Fila, True)
				Next
			End If
		End If
	
	ElseIf KeyDown(KeyControl!) Then
		If This.IsSelected(Row) Then
			This.SelectRow(Row, False)
		Else
			This.SelectRow(Row, True)
		End If
	
	Else
		If This.IsSelected(Row) Then
			This.SelectRow(0, False)
			This.SelectRow(Row, True)
		Else
			This.SelectRow(0, False)
			This.SelectRow(Row, True)
		End If
	End If
End If
end event

event dw_1::rowfocuschanged;//
end event

event dw_1::losefocus;//
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_valofactprod
integer width = 3305
integer height = 464
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_valofactprod
integer x = 3566
integer y = 60
integer taborder = 40
boolean enabled = false
end type

event pb_lectura::clicked;call super::clicked;uo_SelCliente.Bloquear(True)
em_produc.Enabled		=	False
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_valofactprod
integer x = 3566
integer y = 516
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;uo_SelCliente.Bloquear(False)
em_produc.Enabled	=	True	
pb_lectura.Enabled	= 	False
cb_estandar.Enabled	=	False
cb_productor.Enabled	= 	False

em_produc.Text			=	''
sle_nompro.Text			=	''
sle_nomzona.Text			=	''

em_produc.SetFocus()
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_valofactprod
integer x = 3566
integer y = 692
integer taborder = 70
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_valofactprod
integer x = 3566
integer y = 868
integer taborder = 80
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_valofactprod
integer x = 3566
integer y = 1044
integer taborder = 90
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_valofactprod
integer x = 3566
integer y = 1220
integer taborder = 100
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_valofactprod
integer x = 3566
integer y = 1524
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
string text = "Productor"
boolean focusrectangle = false
end type

type em_produc from editmask within w_mant_mues_valofactprod
event modified pbm_enmodified
integer x = 485
integer y = 256
integer width = 219
integer height = 84
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
String		ls_productor, ls_zona
Long		ll_codigo

If This.Text =  '' Then Return 

ll_codigo = Long(This.Text)

SELECT	pro.zona_codigo, pro.prod_nombre, zon.zona_nombre
	INTO 	:li_zona, :ls_productor, :ls_zona
	FROM	dbo.PRODUCTORES as pro, dbo.ZONAS as zon
	WHERE	pro.prod_codigo = :ll_codigo
	AND	zon.zona_codigo = pro.zona_codigo;

If SQLCA.SQLCode = -1 Then
	MessageBox("Error","Error al intentar conección a Base de Datos",Information!, Ok!)
	sle_nompro.text	=	""
	sle_nomzona.text	=	""
	This.Text				=	""
	This.SetFocus()
	Return
ElseIf SQLCA.SQLCode = 100 Then
	MessageBox("Error","Código de Productor no ha sido ingresado.",Information!, Ok!)
	sle_nompro.text	=	""
	sle_nomzona.text	=	""
	This.Text				=	""
	This.SetFocus()
	Return
Else
	istr_mant.argumento[2]	= String(ll_codigo)
	istr_mant.argumento[3]	= ls_productor
	istr_mant.argumento[4]	= String(li_Zona)
	sle_nompro.text			= ls_productor
	sle_nomzona.text			= ls_zona
	pb_lectura.Enabled		= True
	pb_lectura.SetFocus()
END If	
end event

type cb_2 from uo_buscar within w_mant_mues_valofactprod
event clicked pbm_bnclicked
integer x = 722
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
integer x = 837
integer y = 248
integer width = 1705
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
integer x = 485
integer y = 364
integer width = 2071
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
integer x = 485
integer y = 128
integer height = 100
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type cb_estandar from commandbutton within w_mant_mues_valofactprod
integer x = 2597
integer y = 240
integer width = 718
integer height = 108
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
string text = "Copiar Estandar"
end type

event clicked;Long				ll_New, ll_Busqueda
String				ls_Mensaje, ls_Busqueda
Str_Busqueda	lstr_Busq
uo_Variedades	luo_Variedad

luo_Variedad	=	Create uo_Variedades

istr_Mant.Argumento[1]	=	String(uo_SelCliente.Codigo)
istr_Mant.Argumento[2]	=	em_Produc.Text
istr_Mant.Argumento[5]	=	String(uo_SelEspecie.Codigo)


istr_Mant.dw = dw_1
			
OpenWithParm(w_busc_copiavalorizacion, istr_Mant)

lstr_Busq = Message.PowerObjectParm

If UpperBound(lstr_Busq.Argum) > 1 Then	
	ls_Busqueda = "espe_codigo = " + lstr_Busq.Argum[3] + " and " + "vari_codigo = " + lstr_Busq.Argum[4] + " and " + &
							"String(vafa_fecini,'dd/mm/yyyy') = '" + lstr_Busq.Argum[7] + "' and " + &
							"emba_codigo = '" + lstr_Busq.Argum[10] + "'" +" and  emba_tipvid = " + lstr_Busq.Argum[13] + " and " + &
							"colo_nombre = '" + lstr_Busq.Argum[15] + "'" +" and " + &
							"String(vafa_fecter,'dd/mm/yyyy') = '" + lstr_Busq.Argum[8] + "' and " + &
							"vaca_calibr = '" + lstr_Busq.Argum[5] + "'"

	ll_Busqueda = dw_1.Find(ls_Busqueda, 1, dw_1.RowCount())
	
	If ll_Busqueda = 0 Then
		ll_New = dw_1.InsertRow(0)
		
		luo_Variedad.Existe(Integer(lstr_Busq.Argum[3]),Integer(lstr_Busq.Argum[4]), False, SQLCA)
		
		dw_1.Object.clie_codigo[ll_New]		=	uo_SelCliente.Codigo
		dw_1.Object.prod_codigo[ll_New]		=	Long(em_Produc.Text)
		dw_1.Object.espe_codigo[ll_New]		=	Integer(lstr_Busq.Argum[3])
		dw_1.Object.vari_codigo[ll_New]		=	Integer(lstr_Busq.Argum[4])
		dw_1.Object.vari_nombre[ll_New]		=	luo_Variedad.NombreVariedad
		dw_1.Object.vaca_calibr[ll_New]		=	lstr_Busq.Argum[5]
		dw_1.Object.vafp_preuni[ll_New]		=	Dec(lstr_Busq.Argum[6])
		dw_1.Object.vafa_fecini[ll_New]		=	Date(lstr_Busq.Argum[7])
		dw_1.Object.vafa_fecter[ll_New]		=	Date(lstr_Busq.Argum[8])
		dw_1.Object.emba_codigo[ll_New]	=	lstr_Busq.Argum[10]
		dw_1.Object.todos[ll_New]				=	Integer(lstr_Busq.Argum[11])
		dw_1.Object.todoscal[ll_New]			=	Integer(lstr_Busq.Argum[12])
		dw_1.Object.emba_tipvid[ll_New]		=	Integer(lstr_Busq.Argum[13])
		dw_1.Object.semana[ll_New]			=	Integer(lstr_Busq.Argum[14])
		dw_1.Object.colo_nombre[ll_New]	=	lstr_Busq.Argum[15]
	End If
End If

end event

type cb_productor from commandbutton within w_mant_mues_valofactprod
integer x = 2597
integer y = 356
integer width = 718
integer height = 108
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
string text = "Copiar Productor"
end type

event clicked;Long				ll_New, ll_Busqueda
String				ls_Mensaje, ls_Busqueda
Str_Busqueda	lstr_Busq
uo_Variedades	luo_Variedad

luo_Variedad	=	Create uo_Variedades

istr_Mant.Argumento[1]	=	String(uo_SelCliente.Codigo)
istr_Mant.Argumento[2]	=	em_Produc.Text
istr_Mant.Argumento[5]	=	String(uo_SelEspecie.Codigo)

istr_Mant.dw = dw_1
			
OpenWithParm(w_busc_copiavalorizacion_prod, istr_Mant)

lstr_Busq = Message.PowerObjectParm

If UpperBound(lstr_Busq.Argum) > 1 Then	
	ls_Busqueda = "espe_codigo = " + lstr_Busq.Argum[3] + " and " + "vari_codigo = " + lstr_Busq.Argum[4] + " and " + &
							"String(vafa_fecini,'dd/mm/yyyy') = '" + lstr_Busq.Argum[7] + "' and " + &
							"emba_codigo = '" + lstr_Busq.Argum[10] + "'" +" and  emba_tipvid = " + lstr_Busq.Argum[13] + " and " + &
							"colo_nombre = '" + lstr_Busq.Argum[15] + "'" +" and " + &
							"String(vafa_fecter,'dd/mm/yyyy') = '" + lstr_Busq.Argum[8] + "' and " + &
							"vaca_calibr = '" + lstr_Busq.Argum[5] + "'"

	ll_Busqueda = dw_1.Find(ls_Busqueda, 1, dw_1.RowCount())
	
	If ll_Busqueda = 0 Then
		ll_New = dw_1.InsertRow(0)
		
		luo_Variedad.Existe(Integer(lstr_Busq.Argum[3]),Integer(lstr_Busq.Argum[4]), False, SQLCA)
		
		dw_1.Object.clie_codigo[ll_New]		=	uo_SelCliente.Codigo
		dw_1.Object.prod_codigo[ll_New]		=	Long(em_Produc.Text)
		dw_1.Object.espe_codigo[ll_New]		=	Integer(lstr_Busq.Argum[3])
		dw_1.Object.vari_codigo[ll_New]		=	Integer(lstr_Busq.Argum[4])
		dw_1.Object.vari_nombre[ll_New]		=	luo_Variedad.NombreVariedad
		dw_1.Object.vaca_calibr[ll_New]		=	lstr_Busq.Argum[5]
		dw_1.Object.vafp_preuni[ll_New]		=	Dec(lstr_Busq.Argum[6])
		dw_1.Object.vafa_fecini[ll_New]		=	Date(lstr_Busq.Argum[7])
		dw_1.Object.vafa_fecter[ll_New]		=	Date(lstr_Busq.Argum[8])
		dw_1.Object.emba_codigo[ll_New]	=	lstr_Busq.Argum[10]
		dw_1.Object.todos[ll_New]				=	Integer(lstr_Busq.Argum[11])
		dw_1.Object.todoscal[ll_New]			=	Integer(lstr_Busq.Argum[12])
		dw_1.Object.emba_tipvid[ll_New]		=	Integer(lstr_Busq.Argum[13])
		dw_1.Object.semana[ll_New]			=	Integer(lstr_Busq.Argum[14])
		dw_1.Object.colo_nombre[ll_New]	=	lstr_Busq.Argum[15]
	End If
End If

end event

type cb_elimina from commandbutton within w_mant_mues_valofactprod
integer x = 2597
integer y = 124
integer width = 718
integer height = 108
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Elimina MUltiple"
end type

event clicked;Long	ll_Fila

ll_Fila = 1 

Do While ll_Fila <= dw_1.RowCount()
	If dw_1.IsSelected(ll_Fila) Then
		dw_1.DeleteRow(ll_Fila)
	ELse
		ll_Fila++
	End If
Loop
end event

type st_1 from statictext within w_mant_mues_valofactprod
integer x = 1403
integer y = 148
integer width = 251
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
string text = "Especie"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_mant_mues_valofactprod
integer x = 1650
integer y = 128
integer height = 100
integer taborder = 40
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

