$PBExportHeader$w_info_tarjas.srw
forward
global type w_info_tarjas from w_para_informes
end type
type tit_peso from statictext within w_info_tarjas
end type
type dw_1 from datawindow within w_info_tarjas
end type
type dw_2 from datawindow within w_info_tarjas
end type
type dw_3 from datawindow within w_info_tarjas
end type
end forward

global type w_info_tarjas from w_para_informes
integer x = 14
integer y = 32
integer width = 2546
integer height = 1172
boolean titlebar = false
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
tit_peso tit_peso
dw_1 dw_1
dw_2 dw_2
dw_3 dw_3
end type
global w_info_tarjas w_info_tarjas

type variables
str_mant						istr_mant
uo_cliente					iuo_cliente
uo_formato_adhesivos	iuo_formadh
uo_formatosadhesivos	iuo_adhesivo
uo_manejoimpresora		iuo_impresora

Integer						ii_cliente, ii_cantidad, ii_planta
String							is_abreviado
Boolean						ib_impresora
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
public function integer wf_imprime_tarjas ()
public subroutine wf_cargadatos (integer ai_cliente)
end prototypes

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_2.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_2.ResetUpdate()
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function integer wf_imprime_tarjas ();Integer	li_ocx_actual, li_piso, li_techo, li_retorno = 1
Long		ll_actual

dw_3.Reset()
dw_3.DataObject	= 	dw_1.DataObject
				
li_ocx_actual		=	1
li_piso				=	1
li_techo				=	iuo_adhesivo.foad_canocx

If li_techo	> dw_1.RowCount() Then
	li_techo = dw_1.RowCount()
End If

DO WHILE li_piso <= dw_1.RowCount() 
	
	dw_1.RowsCopy(li_piso, li_techo, Primary!, dw_3, 1, Primary!)
	li_piso 	= 	li_techo + 1

	li_techo	=	li_techo + iuo_adhesivo.foad_canocx
	If li_techo	> dw_1.RowCount() Then
		li_techo = dw_1.RowCount()
	End If

	FOR ll_actual = 1 TO dw_3.RowCount()
		CHOOSE CASE li_ocx_actual
			CASE 1
				dw_3.Object.Ole_1.Object.Text		=	String(dw_3.Object.tarjabar[ll_actual])
				dw_3.Object.Ole_1.Visible			=	True
				li_ocx_actual ++
				
			CASE 2
				dw_3.Object.Ole_2.Object.Text		=	String(dw_3.Object.tarjabar[ll_actual])
				dw_3.Object.Ole_2.Visible			=	True
				li_ocx_actual ++
				
			CASE 3
				dw_3.Object.Ole_3.Object.Text		=	String(dw_3.Object.tarjabar[ll_actual])
				dw_3.Object.Ole_3.Visible			=	True
				li_ocx_actual ++
				
			CASE 4
				dw_3.Object.Ole_4.Object.Text		=	String(dw_3.Object.tarjabar[ll_actual])
				dw_3.Object.Ole_4.Visible			=	True
				li_ocx_actual ++
				
			CASE 5
				dw_3.Object.Ole_5.Object.Text		=	String(dw_3.Object.tarjabar[ll_actual])
				dw_3.Object.Ole_5.Visible			=	True
				li_ocx_actual ++
				
			CASE 6
				dw_3.Object.Ole_6.Object.Text		=	String(dw_3.Object.tarjabar[ll_actual])
				dw_3.Object.Ole_6.Visible			=	True
				li_ocx_actual ++
				
			CASE 7
				dw_3.Object.Ole_7.Object.Text		=	String(dw_3.Object.tarjabar[ll_actual])
				dw_3.Object.Ole_7.Visible			=	True
				li_ocx_actual ++
				
		End CHOOSE
		
		If li_ocx_actual > iuo_adhesivo.foad_canocx OR ll_actual = dw_3.RowCount() Then
			li_ocx_actual = 1
			If iuo_impresora.is_impresoracomp <> '' AND ib_impresora Then
				iuo_impresora.setimprcomp()
				If dw_3.Print(False, False) = -1 Then
					MessageBox("Error", "No se pudo realizar la impresión")
				End If
				iuo_impresora.setimprdef()
			Else
				If dw_3.Print(False, False) = -1 Then
					MessageBox("Error", "No se pudo realizar la impresión")
					Return -1
				End If
			End If
			dw_3.Reset()
		End If
	NEXT
LOOP

Return li_retorno
end function

public subroutine wf_cargadatos (integer ai_cliente);Integer	li_fila, respuesta

DO
	ii_cliente	=	ai_cliente
	ii_planta	=	gstr_paramplanta.CodigoPlanta

	li_fila = dw_2.Retrieve(ii_cliente, ii_planta)
	IF li_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF li_fila = 0 THEN
		dw_2.InsertRow(0)
		dw_2.Object.clie_codigo[1] 	=	ii_cliente
		dw_2.Object.plde_codigo[1]	=	ii_planta
	END IF
	
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(THIS)

IF Not IsNull(ii_cantidad)	 	THEN dw_2.Object.crta_cantid[1] = ii_cantidad
IF Not IsNull(is_abreviado) 	THEN dw_2.Object.clie_abrevi[1] = is_abreviado

dw_2.SetColumn("crta_cantid")
end subroutine

on w_info_tarjas.create
int iCurrent
call super::create
this.tit_peso=create tit_peso
this.dw_1=create dw_1
this.dw_2=create dw_2
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tit_peso
this.Control[iCurrent+2]=this.dw_1
this.Control[iCurrent+3]=this.dw_2
this.Control[iCurrent+4]=this.dw_3
end on

on w_info_tarjas.destroy
call super::destroy
destroy(this.tit_peso)
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.dw_3)
end on

event open;call super::open;Integer	li_fila, respuesta

dw_2.SetTransObject(sqlca)

iuo_cliente		=	Create uo_cliente
iuo_formadh	=	Create uo_formato_adhesivos
iuo_adhesivo	=	Create uo_formatosadhesivos
iuo_impresora	=	Create uo_manejoimpresora

SetNull(ii_cantidad)
SetNull(is_abreviado)

iuo_cliente.existe(gi_CodExport, TRUE, sqlca)

//iuo_formadh.Existe(gstr_ParamPlanta.CodigoPlanta, gstr_ParamPlanta.CodigoEspecie, gstr_us.computador, True, SQLCA)
//
//IF IsNull(iuo_formadh.is_impresoracomp) OR Len(iuo_formadh.is_impresoracomp) < 1 THEN
//	MessageBox("Error", "No ha sido asignada una impresora para Tarjas, " + &
//							  "se utilizara un modo de impresión manual, ~r~n" + &
//							  "en donde se deberá seleccionar la impresora una vez por impresión", &
//							  Exclamation!)
//	ib_impresora	=	False
//
//ELSE
//	iuo_impresora.asignaimpresora_comp(iuo_formadh.is_impresoracomp)
//	ib_impresora	=	True
//
//END IF
	
//IF Len(iuo_formadh.ls_formato) > 0 AND iuo_formadh.ls_formato <> '0' THEN
//	IF iuo_adhesivo.Existe(iuo_formadh.ls_formato, True, SQLCa) THEN
//		dw_2.DataObject	=	iuo_formadh.ls_formato
//			
//	ELSE
//		dw_1.DataObject				=	'dw_info_tarjas_nup'
//		iuo_adhesivo.foad_canocx	=	2
//
//	END IF
//ELSE
//	dw_1.DataObject				=	'dw_info_tarjas_nup'
	iuo_adhesivo.foad_canocx	=	1

//END IF
	
wf_cargadatos(gi_CodExport)
end event

type pb_excel from w_para_informes`pb_excel within w_info_tarjas
end type

type st_computador from w_para_informes`st_computador within w_info_tarjas
end type

type st_usuario from w_para_informes`st_usuario within w_info_tarjas
end type

type st_temporada from w_para_informes`st_temporada within w_info_tarjas
end type

type p_logo from w_para_informes`p_logo within w_info_tarjas
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_tarjas
integer x = 265
integer width = 1742
string text = "Impresión de Tarjas Por Cliente"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_tarjas
string tag = "Imprimir Reporte"
integer x = 2098
integer y = 336
integer taborder = 100
end type

event pb_acepta::clicked;Long	fila, ll_cantidad, ll_actual, ll_ultimo, ll_tarja, ll_planta
String	ls_data_object
SetPointer(HourGlass!)

dw_2.AcceptText()
dw_1.Reset()

ii_cantidad		=	dw_2.Object.crta_cantid[1]
is_abreviado	=	dw_2.Object.clie_abrevi[1]
ll_planta			=	gstr_paramplanta.CodigoPlanta

wf_cargadatos(dw_2.Object.clie_codigo[1])

ll_tarja	=	dw_2.Object.crta_numero[1]
IF ll_tarja = 0 OR IsNull(ll_tarja) THEN
	IF MessageBox("Validación", "No Existen tarjas para Este cliente, ~n~r¿Desea crear set de tarjas?", Question!,YesNo!) = 2 THEN
		dw_2.SetColumn("clie_codigo")
		dw_2.SetFocus()
		Return
	ELSE
		ll_tarja	=	0
	END IF
END IF

ll_cantidad	=	dw_2.Object.crta_cantid[1]
IF ll_cantidad = 0 OR IsNull(ll_cantidad) THEN
	MessageBox("Error", "Debe ingresar cantidad de Tarjas a Imprimir")
	Return
END IF

FOR fila = 1 TO (ll_cantidad)
	ll_actual								=	dw_1.InsertRow(0)
	dw_1.Object.cliente[ll_actual]	=	dw_2.Object.clie_abrevi[1]
	ll_ultimo								=	ll_tarja + ll_actual
	dw_1.Object.tarja[ll_actual]		=	ii_Cliente * 100000 + ll_ultimo
	dw_1.Object.tarjabar[ll_actual]	=	String(ii_Cliente * 100000 + ll_ultimo)
NEXT

IF wf_imprime_tarjas() = -1 THEN
	MessageBox("Error", "No se pudo realizar la impresión")
ELSE
	dw_2.Object.crta_numero[1] = ll_ultimo
	dw_2.Object.plde_codigo[1] = ll_planta
	IF wf_actualiza_db() THEN
		w_main.SetMicroHelp("Información Grabada.")
	ELSE
		w_main.SetMicroHelp("No se puede Grabar información.")
		Message.DoubleParm = -1
		RETURN
	END IF
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_tarjas
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2103
integer y = 628
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type tit_peso from statictext within w_info_tarjas
boolean visible = false
integer x = 567
integer y = 1744
integer width = 183
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_info_tarjas
boolean visible = false
integer x = 206
integer y = 972
integer width = 686
integer height = 400
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_adhesivos_nup_100x50"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_info_tarjas
integer x = 270
integer y = 448
integer width = 1751
integer height = 508
integer taborder = 110
boolean bringtotop = true
string dataobject = "dw_sele_info_tarjas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_fila,  ls_Null
String		ls_columna

SetNull(ls_Null)

ls_columna	=	dwo.name

CHOOSE CASE ls_columna
	CASE "clie_codigo"
		IF NOT iuo_Cliente.Existe(Integer(data), TRUE, sqlca) THEN
			This.SetItem(Row, ls_Columna, INteger(ls_Null))
			Return -1
		ELSE
			wf_cargadatos(Integer(data))
		END IF
		
END CHOOSE
end event

type dw_3 from datawindow within w_info_tarjas
boolean visible = false
integer x = 905
integer y = 972
integer width = 686
integer height = 400
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_adhesivos_nup_100x50"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

