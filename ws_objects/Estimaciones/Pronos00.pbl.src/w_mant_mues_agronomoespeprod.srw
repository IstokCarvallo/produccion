$PBExportHeader$w_mant_mues_agronomoespeprod.srw
forward
global type w_mant_mues_agronomoespeprod from w_mant_directo
end type
end forward

global type w_mant_mues_agronomoespeprod from w_mant_directo
integer width = 3909
string title = "Predios por Agrónomos"
boolean resizable = false
end type
global w_mant_mues_agronomoespeprod w_mant_mues_agronomoespeprod

type variables
uo_productores					iou_productores
uo_agronomo_productor		iou_agroespeprod
uo_predios						iuo_predios
uo_especie						iuo_especies
uo_agronomo					iuo_agronomos

DataStore						ids_busc_prodpredio

String	is_sintaxis, is_sentencias[]

end variables

forward prototypes
protected function boolean wf_actualiza_db ()
public function boolean duplicado (string as_columna, string as_valor)
public subroutine wf_buscapredio ()
public subroutine wf_buscaproductor ()
public subroutine wf_buscaagro ()
public subroutine wf_buscaespecie ()
end prototypes

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
String	ls_sentencias[]

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
		
//		ActualizacionRemota(is_sentencias)
		
		is_sentencias	=	ls_sentencias
		
		dw_1.ResetUpdate()
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean duplicado (string as_columna, string as_valor);Boolean	lb_retorno = true
Long		ll_fila, ll_Predio, ll_Productor
Integer	li_Especie

ll_Predio	=	dw_1.Object.prbr_codpre[il_fila]
li_Especie	=	dw_1.Object.espe_codigo[il_fila]
ll_Productor		=	dw_1.Object.prod_codigo[il_fila]

Choose Case as_columna
	Case "espe_codigo"
		li_Especie	=	Integer(as_valor)
		
	Case "prbr_codpre"
		ll_Predio	=	Long(as_valor)
		
	Case "prod_codigo"
		ll_Productor	=	Long(as_valor)
	
End Choose

If IsNull(li_Especie) Or IsNull(ll_Productor) Or Isnull(ll_Predio) Then Return False

ll_fila	=	dw_1.Find("espe_codigo = " + String(li_Especie) + " AND prbr_codpre = " + String(ll_Predio) + &
				" AND prod_codigo = " + String(ll_Productor), 1, dw_1.RowCount())

If ll_fila > 0 and ll_fila <> il_fila Then
	MessageBox("Error","Predio / Productor ya fue ingresado anteriormente",Information!, Ok!)
	lb_retorno = False
End If

Return lb_retorno
end function

public subroutine wf_buscapredio ();str_Busqueda	lstr_busq
Integer li_productor, li_Null

SetNull(li_Null)

lstr_busq.argum[2] = String(dw_1.Object.Prod_codigo[il_fila])

IF Not IsNumber(lstr_busq.argum[2]) THEN lstr_busq.argum[2] = '0'

OpenWithParm(w_busc_prodpredio, lstr_busq)

lstr_busq	= Message.PowerObjectParm

If UpperBound(lstr_busq.Argum) > 3 Then
	If lstr_busq.argum[1] = ""  Or Duplicado("prbr_codpre",lstr_busq.argum[1]) Then
		dw_1.SetColumn("prbr_codpre")
		dw_1.SetFocus()
	Else
		istr_mant.Argumento[4]	=	lstr_busq.Argum[2]
		dw_1.Object.prbr_codpre[il_fila]	= Long(lstr_busq.Argum[1])
		dw_1.Object.prpr_nombre[il_fila]	= lstr_busq.Argum[3]
	
		dw_1.SetFocus()
	End If
End If

RETURN

end subroutine

public subroutine wf_buscaproductor ();str_Busqueda	lstr_busq
Integer li_productor, li_Null

SetNull(li_Null)

lstr_busq.argum[1] = String(-1)

OpenWithParm(w_busc_productores, lstr_busq)

lstr_busq	= Message.PowerObjectParm

If UpperBound(lstr_busq.Argum) > 4 Then
	If lstr_busq.argum[1] = ""  Or Duplicado("prod_codigo",lstr_busq.argum[1]) Then
		dw_1.SetColumn("prod_codigo")
		dw_1.SetFocus()
	Else			
		IF iou_productores.existe(Long(lstr_busq.Argum[1]), false, SQLCA) THEN
			dw_1.Object.prod_codigo[il_Fila]	=	iou_productores.Codigo
			dw_1.Object.prod_nombre[il_Fila]	=	iou_productores.Nombre
			istr_mant.argumento[1]				=	String(iou_productores.Codigo)
		END IF
		dw_1.SetFocus()
	End If
End If

RETURN

end subroutine

public subroutine wf_buscaagro ();str_Busqueda	lstr_busq
Integer li_productor, li_Null

SetNull(li_Null)

lstr_busq.argum[1] = String(dw_1.Object.Agro_codigo[il_fila])

IF Not IsNumber(lstr_busq.argum[1]) THEN lstr_busq.argum[1] = '0'

OpenWithParm(w_busc_agronomos, lstr_busq)

lstr_busq	= Message.PowerObjectParm

If UpperBound(lstr_busq.Argum) > 3 Then
	If lstr_busq.argum[1] = ""  Or Duplicado("agro_codigo",lstr_busq.argum[1]) Then
		dw_1.SetColumn("agro_codigo")
		dw_1.SetFocus()
	Else
		istr_mant.Argumento[1]	=	lstr_busq.Argum[2]
		dw_1.Object.agro_codigo[il_fila]	= Long(lstr_busq.Argum[1])
		dw_1.Object.agro_nombre[il_fila]	= lstr_busq.Argum[2]
	
		dw_1.SetFocus()
	End If
End If

RETURN
end subroutine

public subroutine wf_buscaespecie ();str_Busqueda	lstr_busq
Integer li_productor, li_Null

SetNull(li_Null)

lstr_busq.argum[1] = String(dw_1.Object.Agro_codigo[il_fila])

IF Not IsNumber(lstr_busq.argum[1]) THEN lstr_busq.argum[1] = '0'

OpenWithParm(w_busc_especies, lstr_busq)

lstr_busq	= Message.PowerObjectParm

If UpperBound(lstr_busq.Argum) > 3 Then
	If lstr_busq.argum[1] = ""  Or Duplicado("espe_codigo",lstr_busq.argum[1]) Then
		dw_1.SetColumn("espe_codigo")
		dw_1.SetFocus()
	Else
		istr_mant.Argumento[2]	=	lstr_busq.Argum[3]
		dw_1.Object.espe_codigo[il_fila]	= Long(lstr_busq.Argum[1])
		dw_1.Object.espe_nombre[il_fila]	= lstr_busq.Argum[3]
	
		dw_1.SetFocus()
	End If
End If

RETURN
end subroutine

on w_mant_mues_agronomoespeprod.create
call super::create
end on

on w_mant_mues_agronomoespeprod.destroy
call super::destroy
end on

event open;call super::open;buscar	=	"Código Agrónomo:Nagro_codigo,Código Productor:Nprod_codigo,Código Especie:Nespe_codigo"
ordenar	= 	"Código Agrónomo:agro_codigo,Código Productor:prod_codigo,Código Especie:espe_codigo"

iou_productores						=	Create uo_productores
iou_agroespeprod						=	Create uo_agronomo_productor
iuo_predios								=  Create uo_predios
iuo_especies							=	Create uo_especie
iuo_agronomos							=	Create uo_agronomo

ids_busc_prodpredio					=	Create DataStore

ids_busc_prodpredio.DataObject	=	"dw_busc_prodpredio"
ids_busc_prodpredio.SetTransObject(SQLCa)


end event

event ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve()
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN		
		dw_1.SetRow(1)
		dw_1.SetFocus()
		
		pb_insertar.Enabled	= NOT istr_Mant.Solo_Consulta
		pb_eliminar.Enabled	= NOT istr_Mant.Solo_Consulta
		pb_grabar.Enabled	= NOT istr_Mant.Solo_Consulta
		pb_imprimir.Enabled	= True
		il_fila						= 1
		ias_campo[1]			= String(dw_1.Object.prod_codigo[1])
		
	ELSE
		pb_insertar.Enabled	= NOT istr_Mant.Solo_Consulta
		pb_insertar.SetFocus()
		ias_campo[1]			= ""
		
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetColumn("agro_codigo")
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
String	ls_argumento, ls_argum1
str_info	lstr_info

lstr_info.titulo	= "PREDIOS POR AGRONOMO"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_agronomoespeprod"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve()

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_antesguardar;call super::ue_antesguardar;FOR il_fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(il_fila, 0, Primary!) = NewModified! THEN
		TriggerEvent("ue_validaregistro")
		IF Message.DoubleParm = -1 THEN
			EXIT
		END IF
	END IF
NEXT
end event

event ue_validaregistro();Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.agro_codigo[il_fila]) OR dw_1.Object.agro_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Agrónomo"
	ls_colu[li_cont]	= "agro_codigo"
END IF

IF Isnull(dw_1.Object.espe_codigo[il_fila]) OR dw_1.Object.espe_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~Código Especie"
	ls_colu[li_cont]	= "espe_codigo"
END IF

IF Isnull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~Código Productor"
	ls_colu[li_cont]	= "prod_codigo"
END IF

IF Isnull(dw_1.Object.prbr_codpre[il_fila]) OR dw_1.Object.prbr_codpre[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~Código Predio"
	ls_colu[li_cont]	= "prbr_codpre"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_guardar();IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN Return

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_agronomoespeprod
boolean visible = false
integer x = 0
integer y = 0
integer width = 78
integer height = 72
integer weight = 400
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_agronomoespeprod
integer x = 3543
integer y = 388
integer taborder = 50
end type

event pb_nuevo::clicked;call super::clicked;pb_imprimir.Enabled	= False
pb_insertar.Enabled	= False
pb_grabar.Enabled	= False
pb_eliminar.Enabled	= False
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_agronomoespeprod
integer x = 3534
integer y = 152
integer taborder = 30
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_agronomoespeprod
integer x = 3534
integer y = 744
integer taborder = 70
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_agronomoespeprod
integer x = 3534
integer y = 564
integer taborder = 60
end type

event pb_insertar::clicked;call super::clicked;pb_grabar.enabled= true
pb_eliminar.enabled= true
end event

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_agronomoespeprod
integer x = 3534
integer y = 1420
integer taborder = 100
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_agronomoespeprod
integer x = 3534
integer y = 1104
integer taborder = 90
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_agronomoespeprod
integer x = 3534
integer y = 924
integer taborder = 80
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_agronomoespeprod
integer x = 87
integer y = 64
integer width = 3301
integer height = 1756
integer taborder = 40
string dataobject = "dw_mues_agronomoespeprod"
boolean hscrollbar = true
end type

event dw_1::itemchanged;String	ls_Columna
Integer	li_null, li_productor, li_fila

ls_Columna	=	dwo.Name
SetNull(li_null)

Choose Case ls_Columna
	Case 'agro_codigo'
		If Not iuo_Agronomos.Existe(Integer(data), True, Sqlca) Then
			This.SetItem(row, ls_Columna, li_null)
			Return 1
		Else
			This.Object.agro_nombre[Row] = iuo_Agronomos.Nombre
		End If

	Case "espe_codigo"
		If Not iuo_especies.Existe(Integer(data),True,SQLCA) Or  Duplicado(ls_Columna, data) Then
			This.SetItem(row, ls_Columna, li_null)
			Return 1
		Else
			This.Object.espe_nombre[Row] = iuo_Especies.Nombre
		End If
		
	CASE "prod_codigo"
		IF iou_productores.Existe(Long(data), True, SQLCA) THEN
			ids_busc_prodpredio.Retrieve(Integer(data))
			dw_1.Object.prod_codigo[row]	=	iou_productores.Codigo
			dw_1.Object.prod_nombre[row]	=	iou_productores.Nombre
		ELSE
			This.Object.Prod_codigo[row]	=	li_null
			dw_1.Object.prod_nombre[row]	=	String(li_null)
			Return 1
		END IF
		
	CASE "prbr_codpre"
		li_fila	=	ids_busc_prodpredio.Find("prpr_codigo = " + Data, 1, ids_busc_prodpredio.RowCount())
		IF li_fila > 0 THEN
			dw_1.Object.prbr_codpre[row]	=  ids_busc_prodpredio.Object.prpr_codigo[li_fila]
			dw_1.Object.prpr_nombre[row]	=  ids_busc_prodpredio.Object.prpr_nombre[li_fila]
		ELSE
			MessageBox("Error", "El Codigo de predio ingresado no existe")
			This.Object.prbr_codpre[row]	=	li_null
			dw_1.Object.prpr_nombre[row]	=  String(li_null)
			Return 1
		END IF
End Choose
end event

event dw_1::buttonclicked;call super::buttonclicked;String	ls_Boton

ls_Boton = 	dwo.Name

Choose Case ls_Boton
		
	Case "b_agronomo"
		wf_BuscaAgro()

	Case "b_especie"
		wf_BuscaEspecie()
		
	Case "b_prod"
		wf_buscaproductor()

	Case "buscapredio"
		wf_BuscaPredio()
		
End Choose 

end event

event dw_1::itemerror;call super::itemerror;	Return 1
end event

event dw_1::sqlpreview;IF request = PreviewFunctionUpdate!	THEN
	is_sintaxis	=	sqlsyntax
END IF

CALL Super::SqlPreview
end event

event dw_1::on_delete;call super::on_delete;Integer	li_sentencias

li_sentencias	=	UpperBound(is_sentencias)

li_sentencias ++

is_sentencias[li_sentencias] = is_sintaxis
end event

event dw_1::on_insert;call super::on_insert;Integer	li_sentencias

li_sentencias	=	UpperBound(is_sentencias)

li_sentencias ++

is_sentencias[li_sentencias] = is_sintaxis
end event

event dw_1::on_update;call super::on_update;Integer	li_sentencias

li_sentencias	=	UpperBound(is_sentencias)

li_sentencias ++

is_sentencias[li_sentencias] = is_sintaxis
end event

