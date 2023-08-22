$PBExportHeader$w_mant_deta_captura_archivocajas.srw
forward
global type w_mant_deta_captura_archivocajas from w_mant_detalle
end type
end forward

global type w_mant_deta_captura_archivocajas from w_mant_detalle
integer width = 3104
integer height = 2160
end type
global w_mant_deta_captura_archivocajas w_mant_deta_captura_archivocajas

type variables
DatawindowChild    idwc_cliente, idwc_planta, idwc_especie, idwc_etiqueta, idwc_variedad, &
		idwc_categoria, idwc_productor, idwc_predio, idwc_cuartel
end variables

forward prototypes
public function boolean noexisteendddw (string as_columna, string as_valor, string as_nombre, boolean ab_string)
public function boolean duplicado (string as_columna, string as_valor)
public subroutine wf_opciontodas ()
public function boolean noexistecalibre (string as_calibre)
end prototypes

public function boolean noexisteendddw (string as_columna, string as_valor, string as_nombre, boolean ab_string);DataWindowChild	ldwc_1

String	ls_busca
Long		ll_fila

dw_1.GetChild(as_columna,ldwc_1)

IF ab_string THEN
	ls_busca	=	as_columna + " = '" + as_valor + "'"
ELSE
	ls_busca	=	as_columna + " = " + as_valor
END IF

IF Isnull(ls_busca) THEN
	ll_fila = 1
ELSE
	ll_fila	=	ldwc_1.Find(ls_busca,1,ldwc_1.RowCount())
END IF

IF ll_fila = 0 THEN
	MessageBox("Atención ", as_nombre + " no ha sido ingresado.", Exclamation!, OK!) 
	RETURN True
ELSE
	IF as_nombre = 'Variedad' THEN
		dw_1.SetItem(il_fila,"variedades_vari_nombre",ldwc_1.GetItemString(ll_fila,"vari_nombre"))
	ELSEIF as_nombre = 'Tipo de Pallet' THEN
		dw_1.SetItem(il_fila,"tpem_cancaj",ldwc_1.GetItemNumber(ll_fila,"tpem_cancaj"))
	END IF
	RETURN False
END IF
end function

public function boolean duplicado (string as_columna, string as_valor);Long		ll_fila
String	ls_especie, ls_variedad, ls_embalaje, ls_tipopalemb, ls_calibre

ls_especie		=	String(dw_1.Object.espe_codigo[il_fila])
ls_variedad		=	String(dw_1.Object.vari_codigo[il_fila])
ls_embalaje		=	dw_1.Object.emba_codigo[il_fila]
ls_tipopalemb	=	dw_1.Object.tpem_codigo[il_fila]
ls_calibre		=	dw_1.Object.inpe_calibr[il_fila]

CHOOSE CASE as_columna
	CASE "espe_codigo"
		ls_especie	=	as_valor
		
	CASE "vari_codigo"
		ls_variedad	=	as_valor
		
	CASE "emba_codigo"
		ls_embalaje		=	as_valor
		
	CASE "tpem_codigo"
		ls_tipopalemb	=	as_valor

	CASE "inpe_calibr"
		ls_calibre		=	as_valor		
		
END CHOOSE

IF IsNull(ls_Especie) OR ls_Especie = "" THEN
	ls_Especie	=	""
ELSE
	ls_Especie	=	"espe_codigo = " + ls_Especie + " and "
END IF

IF IsNull(ls_Variedad) OR ls_Variedad = "" THEN
	ls_Variedad	=	""
ELSE
	ls_Variedad	=	"vari_codigo = " + ls_Variedad + " and "
END IF

IF IsNull(ls_calibre) OR ls_calibre = "" THEN
	ls_calibre	=	""
ELSE
	ls_calibre	=	"inpe_calibr = '" + ls_calibre + "' and "
END IF

ll_fila	= istr_mant.dw.Find(ls_especie + ls_variedad + ls_calibre + &
							"emba_codigo = '" + ls_embalaje + "' and " + &
							"tpem_codigo = '" + ls_tipopalemb + "'", &	
							1, istr_mant.dw.RowCount())

IF ll_fila > 0 and ll_fila <> il_FilaAnc THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine wf_opciontodas ();Long		ll_fila
Integer	li_null
String	ls_null

SetNull(li_null)
SetNull(ls_null)

//Inserción de Opción Todas

//ll_fila	= dwc_especie.InsertRow(1)
//dwc_especie.SetItem(ll_fila,"espe_codigo",li_null)
//dwc_especie.SetItem(ll_fila,"espe_nombre","TODAS")
//
//ll_fila	= dwc_variedad.InsertRow(1)
//dwc_variedad.SetItem(ll_fila,"vari_codigo",li_null)
//dwc_variedad.SetItem(ll_fila,"vari_nombre","TODAS")
end subroutine

public function boolean noexistecalibre (string as_calibre);String		ls_nombre
Integer		li_Cliente, li_Especie, li_Variedad

li_Cliente	=	Integer(istr_mant.Argumento[1])
li_Especie	=	dw_1.Object.espe_codigo[il_Fila]
li_Variedad	=	dw_1.Object.vari_codigo[il_Fila]

IF IsNull(li_Especie) = False OR li_Especie > 0 OR &
	IsNull(li_Variedad) = False OR li_Variedad > 0 OR &
	IsNull(as_Calibre) = False OR as_Calibre = '' THEN
	SELECT	vaca_calibr
		INTO	:ls_nombre
		FROM	dba.variecalibre
		WHERE	espe_codigo	=	:li_Especie
		AND	vari_codigo =	:li_Variedad
		AND	vaca_calibr =	:as_Calibre;

	IF sqlca.SQLCode = -1 THEN
  		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla variecalibre")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Calibre no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
		
		RETURN True
	ELSE
		
		RETURN False
	END IF
ELSE
	RETURN True
END IF
end function

on w_mant_deta_captura_archivocajas.create
call super::create
end on

on w_mant_deta_captura_archivocajas.destroy
call super::destroy
end on

event open;call super::open;

dw_1.GetChild("clie_codigo",idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
IF idwc_cliente.Retrieve() = 0 THEN
   idwc_cliente.InsertRow(0)
END IF

dw_1.GetChild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(SQLCA)
IF idwc_planta.Retrieve() = 0 THEN
   idwc_planta.InsertRow(0)
END IF

dw_1.GetChild("espe_codigo",idwc_especie)
idwc_especie.SetTransObject(SQLCA)
IF idwc_especie.Retrieve() = 0 THEN
   idwc_especie.InsertRow(0)
END IF

dw_1.GetChild("etiq_codigo",idwc_etiqueta)
idwc_etiqueta.SetTransObject(SQLCA)
IF idwc_etiqueta.Retrieve() = 0 THEN
   idwc_etiqueta.InsertRow(0)
END IF

dw_1.GetChild("vari_codigo",idwc_variedad)
idwc_variedad.SetTransObject(SQLCA)
IF idwc_variedad.Retrieve(dw_1.Object.espe_codigo[1]) = 0 THEN
   idwc_variedad.InsertRow(0)
END IF

dw_1.GetChild("capr_varrot",idwc_variedad)
idwc_variedad.SetTransObject(SQLCA)
IF idwc_variedad.Retrieve(dw_1.Object.espe_codigo[1]) = 0 THEN
   idwc_variedad.InsertRow(0)
END IF

dw_1.GetChild("cate_codigo",idwc_categoria)
idwc_categoria.SetTransObject(SQLCA)
IF idwc_categoria.Retrieve() = 0 THEN
   idwc_categoria.InsertRow(0)
END IF

dw_1.GetChild("prod_codigo",idwc_productor)
idwc_productor.SetTransObject(SQLCA)
IF idwc_productor.Retrieve() = 0 THEN
   idwc_productor.InsertRow(0)
END IF

dw_1.GetChild("prod_predio",idwc_predio)
idwc_predio.SetTransObject(SQLCA)
IF idwc_predio.Retrieve() = 0 THEN
   idwc_predio.InsertRow(0)
END IF

dw_1.GetChild("prod_cuarte",idwc_cuartel)
idwc_cuartel.SetTransObject(SQLCA)
IF idwc_cuartel.Retrieve() = 0 THEN
   idwc_cuartel.InsertRow(0)
END IF

dw_1.GetChild("capr_cespak",idwc_planta)
idwc_planta.SetTransObject(SQLCA)
IF idwc_planta.Retrieve() = 0 THEN
   idwc_planta.InsertRow(0)
END IF

end event

event ue_recuperadatos;//
w_main.SetMicroHelp("Recuperando Datos...")

SetPointer(HourGlass!)
PostEvent("ue_listo")

IF istr_mant.Agrega THEN
	pb_Cancela.Enabled	=	False
	pb_Primero.Enabled	=	False
	pb_Anterior.Enabled	=	False
	pb_Siguiente.Enabled	=	False
	pb_Ultimo.Enabled		=	False
	
	wf_nuevo()
	This.Title			= "INGRESO DE REGISTRO"
ELSE
	IF dw_1.RowCount() > 1 THEN
		pb_Primero.Enabled	=	True
		pb_Anterior.Enabled	=	True
		pb_Siguiente.Enabled	=	True
		pb_Ultimo.Enabled		=	True
	END IF
	
	il_fila	=	istr_mant.dw.GetRow()
	
	dw_1.SetRedraw(False)
	dw_1.SetRow(il_fila)
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRedraw(True)

	IF istr_mant.Borra THEN
		dw_1.Enabled		=	False
		pb_Salir.Enabled	=	False
		This.Title			=	"ELIMINACION DE REGISTRO"
	ELSEIF istr_mant.Solo_Consulta THEN
		dw_1.Enabled			=	False
		pb_Acepta.Enabled		=	False
		pb_Cancela.Enabled	=	False
		This.Title				=	"CONSULTA DE REGISTRO"
	ELSE
		pb_Salir.Enabled	=	False
		This.Title			=	"VISUALIZACIÓN DE REGISTRO"
	END IF
END IF
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_captura_archivocajas
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_captura_archivocajas
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_captura_archivocajas
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_captura_archivocajas
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_captura_archivocajas
boolean visible = false
integer x = 2807
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_captura_archivocajas
integer x = 2807
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_captura_archivocajas
boolean visible = false
integer x = 2807
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_captura_archivocajas
integer width = 2574
integer height = 1876
string dataobject = "dw_mant_captura_archivocajas"
end type

