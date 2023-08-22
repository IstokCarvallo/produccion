$PBExportHeader$w_mant_mues_pesocosecha.srw
$PBExportComments$Ventana de Asociación de Categorías por Especie
forward
global type w_mant_mues_pesocosecha from w_mant_directo
end type
type st_1 from statictext within w_mant_mues_pesocosecha
end type
type uo_selespecie from uo_seleccion_especie within w_mant_mues_pesocosecha
end type
end forward

global type w_mant_mues_pesocosecha from w_mant_directo
string tag = "Mantenedor Peso Cosecha"
integer width = 2075
integer height = 1956
string title = "Peso Cosecha"
st_1 st_1
uo_selespecie uo_selespecie
end type
global w_mant_mues_pesocosecha w_mant_mues_pesocosecha

type variables
datawindowchild 	idwc_variedad
String					is_sentencias[], is_Sintaxis

uo_variedades iuo_variedad
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
public function boolean duplicado (integer ai_variedad)
end prototypes

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
String		ls_sentencias[]

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

public function boolean duplicado (integer ai_variedad);Long		ll_fila
	
ll_fila	= dw_1.Find("vari_codigo = " + string(ai_variedad), 1, dw_1.RowCount())
	
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Código de Categoria ya Fue Ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
		
end function

event open;call super::open;Boolean lb_Cerrar

If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelEspecie.Seleccion(False, False)
	uo_SelEspecie.Inicia(gi_CodEspecie)
	
	dw_1.GetChild("vari_codigo", idwc_variedad)
	idwc_variedad.SetTransObject(Sqlca)
	idwc_variedad.Retrieve(uo_SelEspecie.Codigo)
	
	iuo_variedad				=	Create uo_variedades 
		
	buscar			= "Código Variedad:Nvari_codigo"
	ordenar			= "Código Variedad:vari_codigo"
	is_ultimacol		= "pcva_pescos"
End If
end event

on w_mant_mues_pesocosecha.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selespecie=create uo_selespecie
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selespecie
end on

on w_mant_mues_pesocosecha.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selespecie)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Integer li_Null
Long ll_fila, respuesta

SetNull(li_Null)
	
DO
	ll_Fila	= dw_1.Retrieve(uo_SelEspecie.Codigo)

	IF ll_Fila = -1 THEN
		Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_Fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
	ELSE
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
	END IF

LOOP WHILE Respuesta = 1

IF Respuesta = 2 THEN Close(This)
end event

event ue_imprimir;SetPointer(HourGlass!)
Integer	li_Null, li_respuesta
Long		fila
String	ls_argumento, ls_argum1

SetNull(li_Null)
str_info	lstr_info

li_respuesta	=	MessageBox("Informe de Peso Cosecha.","Desea imprimir Todas las Especies?",Question!,YesNo!,1)
									  
lstr_info.titulo	= "PESO COSECHA"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_pesocosecha"
vinf.dw_1.SetTransObject(sqlca)

IF li_respuesta = 2 THEN
	Fila	= vinf.dw_1.Retrieve(uo_SelEspecie.Codigo)
ELSE
	Fila	= vinf.dw_1.Retrieve(-1)
END IF

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
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

event ue_validaregistro;call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 		= ls_mensaje + "~nCódigo de Variedad"
	ls_colu[li_cont]	= "vari_codigo"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_antesguardar;Long	ll_fila = 1

DO WHILE ll_fila <= dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = New! THEN
		dw_1.DeleteRow(ll_fila)
	ELSEIF dw_1.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
		IF NOT Isnull(dw_1.Object.vari_codigo[ll_fila]) THEN
			dw_1.Object.espe_codigo[ll_fila] = uo_SelEspecie.Codigo
			ll_fila ++
		ELSE
			dw_1.DeleteRow(ll_fila)
		END IF
	ELSE
		ll_fila ++		
	END IF
LOOP
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_pesocosecha
integer x = 96
integer width = 1417
integer height = 220
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_pesocosecha
integer x = 1669
integer y = 432
integer taborder = 30
end type

event pb_nuevo::clicked;call super::clicked;Integer li_Null

SetNull(li_Null)

uo_SelEspecie.Bloquear(False)

pb_insertar.Enabled	= 	False
pb_eliminar.Enabled	= 	False
pb_grabar.Enabled	= 	False
pb_imprimir.Enabled	= 	False
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_pesocosecha
integer x = 1669
integer y = 136
integer taborder = 20
end type

event pb_lectura::clicked;call super::clicked;uo_SelEspecie.Bloquear(True)
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_pesocosecha
integer x = 1669
integer y = 792
integer taborder = 60
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_pesocosecha
integer x = 1669
integer y = 612
integer taborder = 50
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_pesocosecha
integer x = 1669
integer y = 1536
integer taborder = 90
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_pesocosecha
integer x = 1669
integer y = 1152
integer taborder = 80
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_pesocosecha
integer x = 1669
integer y = 972
integer taborder = 70
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_pesocosecha
integer x = 91
integer y = 320
integer width = 1417
integer height = 1432
integer taborder = 40
string dataobject = "dw_mant_mues_pesocosecha"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna 
Integer	li_Null

SetNull(li_Null)

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "vari_codigo"
	  	If Not iuo_variedad.Existe(uo_SelEspecie.Codigo, Integer(data), TRUE, SQLCA) Or Duplicado(Integer(data)) Then
		  This .SetItem(row, ls_Columna ,li_Null) 
		  Return 1		  
	End If
		
End Choose 
		
		
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

type st_1 from statictext within w_mant_mues_pesocosecha
integer x = 165
integer y = 144
integer width = 288
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
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_mant_mues_pesocosecha
event destroy ( )
integer x = 453
integer y = 132
integer height = 88
integer taborder = 30
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

dw_1.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)

Choose Case This.Codigo
	Case -1, -9
		idwc_variedad.Retrieve(-1)
		
	Case Else
		idwc_variedad.Retrieve(This.Codigo)
		
End Choose
end event

