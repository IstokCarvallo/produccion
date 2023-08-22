$PBExportHeader$w_mant_modificadetallefruta.srw
$PBExportComments$Mantención Detalle de Pallets a Inspeccionar accesado directamente desde el Menu.
forward
global type w_mant_modificadetallefruta from w_mant_directo
end type
type st_2 from statictext within w_mant_modificadetallefruta
end type
type st_1 from statictext within w_mant_modificadetallefruta
end type
type em_numero from editmask within w_mant_modificadetallefruta
end type
type st_3 from statictext within w_mant_modificadetallefruta
end type
type dw_3 from datawindow within w_mant_modificadetallefruta
end type
type dw_4 from datawindow within w_mant_modificadetallefruta
end type
end forward

global type w_mant_modificadetallefruta from w_mant_directo
integer x = 155
integer y = 156
integer width = 4009
integer height = 2100
string title = "MANTENEDOR DETALLE PALLET"
event ue_validaborrar ( )
st_2 st_2
st_1 st_1
em_numero em_numero
st_3 st_3
dw_3 dw_3
dw_4 dw_4
end type
global w_mant_modificadetallefruta w_mant_modificadetallefruta

type variables
DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta
Integer ii_pallet, ii_nuevo, ii_tipo, ii_contador
String	is_report

uo_plantadesp			iuo_Planta
uo_clientesprod			iuo_ClienteProd
uo_calibre	iuo_calibre



end variables

forward prototypes
public function boolean noexistevariedad (string as_columna, string as_valor)
public function string f_variedadnom (integer codigo)
public function string f_prodnombre (integer li_codigo)
public subroutine buscavariedad ()
public function boolean existepallet (long al_numero)
public function boolean existeembalaje (string as_embalaje, integer al_cliente)
public function boolean existeproductor (long al_productor)
public function boolean existepacking (integer ai_planta)
public function boolean existepredio (integer ai_predio, long al_productor)
end prototypes

public function boolean noexistevariedad (string as_columna, string as_valor);Integer	li_Especie, li_Variedad, li_Existes
Boolean	lb_Retorno


li_Variedad	=	dw_1.Object.pafr_varrot[il_Fila]



li_Variedad	=	Integer(as_Valor)

SELECT Count(*)
INTO	 :li_Existes
FROM	 dbo.variedades
WHERE  vari_codigo	=	:li_Variedad;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de Variedades")
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de Variedad no Existe.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF


RETURN lb_Retorno
end function

public function string f_variedadnom (integer codigo);string ls_nombre
integer li_cliente, li_especie

li_cliente = dw_1.Object.clie_codigo[il_fila]
li_especie = dw_1.Object.espe_codigo[il_fila]

SELECT	vari_nombre
	INTO	:ls_nombre
	FROM	dbo.variedades
	WHERE	vari_codigo	=	:codigo and
		   espe_codigo =	:li_especie;
		

	RETURN ls_nombre

end function

public function string f_prodnombre (integer li_codigo);string ls_nombre
integer li_cliente, li_especie

li_cliente = dw_1.Object.clie_codigo[il_fila]


SELECT	prod_nombre
	INTO	:ls_nombre
	FROM	dbo.productores
	WHERE	prod_codigo	=	:li_codigo;
		
RETURN ls_nombre



end function

public subroutine buscavariedad ();string 	ls_null

SetNull(ls_null)

Str_busqueda	lstr_busq

dw_1.Modify("buscavariedad.border = 0")
dw_1.Modify("buscavariedad.border = 5")

lstr_busq.argum[1]	=	istr_mant.Argumento[1]
lstr_busq.argum[2]	=	String(dw_1.GetItemNumber(1, "espe_codigo"))

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[4] <> '' THEN
	istr_mant.argumento[4]	=	lstr_busq.argum[4]
	istr_mant.argumento[5]	=	lstr_busq.argum[5]
	
	dw_1.setItem(il_fila, "pafr_varrot", Integer(lstr_busq.argum[4]))
	dw_1.setItem(il_fila, "vari_nombre_rotula", lstr_busq.argum[5])
	dw_1.SetColumn("pafr_varrot")
	dw_1.SetFocus()
END IF

end subroutine

public function boolean existepallet (long al_numero);Integer	li_codexp, li_planta, li_Tipova
Date		ld_fecha
String	ls_Embarque
Long		ll_pallet

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF Upper(gstr_us.Nombre) <> Upper('EOrdenes')  THEN 

	IF al_numero <> 0 OR li_planta = 0 THEN
	
			SELECT pan.paen_numero
			INTO	:ll_pallet
			FROM	dbo.palletencab as pan,dbo.despafrigode as det,dbo.despafrigoen as enc
			WHERE	pan.plde_codigo =	:li_planta
			AND	pan.clie_codigo =	:li_codexp
			AND	pan.paen_numero =	:al_numero
			AND	pan.paen_Estado  in (2,8)
			AND	pan.clie_codigo = det.clie_codigo
			AND	pan.plde_codigo = det.plde_codigo
			AND	pan.paen_numero = det.paen_numero
			AND	pan.clie_codigo = det.clie_codigo
			AND	pan.plde_codigo = enc.plde_codigo
			AND	det.defe_numero = enc.defe_numero
			AND 	enc.defe_tiposa in (7,8,9);
					
		IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Palletencab")
			em_numero.Text = ''
			em_numero.SetFocus()
			RETURN False
		ELSEIF sqlca.SQLCode = 100 THEN
			MessageBox("Atención", "No existe Número de Pallet Indicado o no está Despachado.~r~rIngrese otro Número.", &
							Exclamation!, Ok!)
			em_numero.Text = ''
			em_numero.SetFocus()
			RETURN False
		END IF
		RETURN TRUE
	END IF	
END IF		
end function

public function boolean existeembalaje (string as_embalaje, integer al_cliente);Integer	li_Existes
Boolean	lb_Retorno


SELECT Count(*)
INTO	 :li_Existes
FROM	 dbo.embalajesprod
WHERE  emba_codigo =	:as_embalaje
AND	 clie_codigo  = :al_cliente;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de embalajesprod")
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de Embalaje no Existe.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF


RETURN lb_Retorno
end function

public function boolean existeproductor (long al_productor);Integer	li_Existes, li_cliente
Boolean	lb_Retorno
Long		ll_productor

ll_productor	= al_productor
li_cliente		= dw_3.Object.clie_codigo[1]

SELECT Count(*)
INTO	 :li_Existes
FROM	 dbo.productores as pro,dbo.productoresclientes as cli
WHERE  pro.prod_codigo	=	:ll_productor
AND	pro.prod_codigo = cli.prod_codigo
AND	cli.clie_codigo = :li_cliente;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de Productores")
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de productor no existe o pertenece a otro cliente.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF


RETURN lb_Retorno
end function

public function boolean existepacking (integer ai_planta);Integer	li_Existes
Boolean	lb_Retorno
Long		ll_productor

ai_planta	=	ai_planta

SELECT Count(*)
INTO	 :li_Existes
FROM	 dbo.plantadesp
WHERE  plde_codigo	=	:ai_planta;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de plantadesp")
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de Packing no Existe.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF


RETURN lb_Retorno
end function

public function boolean existepredio (integer ai_predio, long al_productor);Integer	li_cont
Long	   registros

dw_1.accepttext()

SELECT	count(*)
	INTO	:li_cont
	FROM  dbo.spro_prodpredio
	WHERE	prod_codigo =	:al_productor
	AND	prpr_codigo =	:ai_predio;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_prodpredio")
	RETURN True
ELSEIF li_cont = 0 THEN
	MessageBox("Atención", "Predio No existe Para este Productor, Ingrese otro.", &
					Exclamation!, OK!)
	RETURN True
ELSE
	//dw_1.SetItem(il_fila, "pafr_calibr", ls_calibr)
	RETURN False
END IF

end function

event open;x				= 0
y				= 0
//This.Width	= dw_1.width + 540
//This.Height	= 1993
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
								
/* La asignación a las siguientes variables no se hereda, debe ser adicional en ventana descendiente */
buscar			= "Código:Ncodigo,Descripción:Sconcepto"
ordenar			= "Código:codigo,Descripción:concepto"
is_ultimacol	= "columna"


ii_tipo	=	Integer(Message.StringParm)

dw_3.SetTransObject(sqlca)
dw_3.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_3.InsertRow(0)
dw_3.SetItem(1,"clie_codigo", gi_codexport)

dw_4.SetTransObject(sqlca)
dw_4.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_4.InsertRow(0)
dw_4.SetItem(1,"plde_codigo", gi_codplanta)

istr_mant.argumento[1] =String(gi_codexport)
istr_mant.argumento[2] =String(gi_codplanta)

iuo_Planta				=	Create uo_plantadesp
iuo_ClienteProd		=	Create uo_clientesprod
iuo_calibre   			=	Create uo_calibre
end event

on w_mant_modificadetallefruta.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_1=create st_1
this.em_numero=create em_numero
this.st_3=create st_3
this.dw_3=create dw_3
this.dw_4=create dw_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.em_numero
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.dw_3
this.Control[iCurrent+6]=this.dw_4
end on

on w_mant_modificadetallefruta.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_1)
destroy(this.em_numero)
destroy(this.st_3)
destroy(this.dw_3)
destroy(this.dw_4)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(long(istr_mant.argumento[6]),integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))
	
	IF ll_fila	= -1 THEN
		respuesta	= MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta	 = 1

IF respuesta	= 2 THEN 
	Close(This)
ELSE	
	pb_insertar.Enabled = True
END IF














end event

event ue_imprimir;SetPointer(HourGlass!)

Long		ll_Filas
str_info	lstr_info

lstr_info.titulo	= "DETALLE DE PALLET"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_rotulacion_pallet"

vinf.dw_1.SetTransObject(sqlca)

ll_Filas	=	vinf.dw_1.Retrieve(long(istr_mant.argumento[6]),integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))

IF ll_Filas = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Filas = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_validaregistro;call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1

		
IF dw_1.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE	
	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nNo existen mas Registros"
		ls_colu[li_cont]	= "paen_numero"
	END IF

	IF IsNull(dw_1.Object.pafr_varrot[il_fila]) OR dw_1.Object.pafr_varrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Variedad Rotulada"
		ls_colu[li_cont]	= "pafr_varrot"
	END IF
	
	IF IsNull(dw_1.Object.pafr_prdrot[il_fila]) OR dw_1.Object.pafr_prdrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Productor Rotulado"
		ls_colu[li_cont]	= "pafr_prdrot"
	END IF
	
	IF IsNull(dw_1.Object.pafr_calrot[il_fila]) OR dw_1.Object.pafr_calrot[il_fila] = ''	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Calibre Rotulado"
		ls_colu[li_cont]	= "pafr_calrot"
	END IF
	
	IF IsNull(dw_1.Object.pafr_huert4[il_fila]) OR dw_1.Object.pafr_huert4[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Predio Rotulado"
		ls_colu[li_cont]	= "pafr_huert4"
	END IF	

	IF IsNull(dw_1.Object.pafr_cuart4[il_fila]) OR dw_1.Object.pafr_cuart4[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Cuartel Rotulado"
		ls_colu[li_cont]	= "pafr_cuart4"
	END IF	

	IF IsNull(dw_1.Object.pafr_rotpak[il_fila]) OR dw_1.Object.pafr_rotpak[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Packing Rotulado"
		ls_colu[li_cont]	= "pafr_rotpak"
	END IF	
	
	IF dw_1.Object.pafr_fecrot[il_fila] = Date('19000101')	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de fecha embalaje Rotulada"
		ls_colu[li_cont]	= "pafr_fecrot"
	END IF		
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	
//		ii_contador = 1
//		
//		do while ii_contador <= dw_1.RowCount() and dw_1.Object.Paen_numero[ii_contador] <> 0
//			ii_contador++
//		loop 
//		ii_contador --
//		dw_1.SetRow(ii_contador)
//		Message.DoubleParm = -1
	END IF
END IF





end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1

		
//IF dw_1.rowcount() = 0 THEN
//	pb_grabar.Enabled	=	False 
//ELSE	
//	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nNo existen mas Registros"
//		ls_colu[li_cont]	= "paen_numero"
//	END IF
//	
//	IF IsNull(dw_1.Object.pafr_varrot[il_fila]) OR dw_1.Object.pafr_varrot[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Variedad Rotulada"
//		ls_colu[li_cont]	= "pafr_varrot"
//	END IF
//	
//	IF IsNull(dw_1.Object.pafr_prdrot[il_fila]) OR dw_1.Object.pafr_prdrot[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Productor Rotulado"
//		ls_colu[li_cont]	= "pafr_prdrot"
//	END IF
//	
//	IF IsNull(dw_1.Object.pafr_calrot[il_fila]) OR dw_1.Object.pafr_calrot[il_fila] = ''	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Calibre Rotulado"
//		ls_colu[li_cont]	= "pafr_calrot"
//	END IF
//	
//	IF IsNull(dw_1.Object.pafr_huert4[il_fila]) OR dw_1.Object.pafr_huert4[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Predio Rotulado"
//		ls_colu[li_cont]	= "pafr_huert4"
//	END IF	
//
//	IF IsNull(dw_1.Object.pafr_cuart4[il_fila]) OR dw_1.Object.pafr_cuart4[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Cuartel Rotulado"
//		ls_colu[li_cont]	= "pafr_cuart4"
//	END IF	
//
//	IF IsNull(dw_1.Object.pafr_rotpak[il_fila]) OR dw_1.Object.pafr_rotpak[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Packing Rotulado"
//		ls_colu[li_cont]	= "pafr_rotpak"
//	END IF	
//	
//	IF dw_1.Object.pafr_fecrot[il_fila] = Date('19000101')	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Fecha Embalaje Rotulada"
//		ls_colu[li_cont]	= "pafr_fecrot"
//	END IF		
//	
//	IF li_cont > 0 THEN
//		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
//		dw_1.SetColumn(ls_colu[1])
//		dw_1.SetFocus()
//	END IF
//END IF
//
//
//
//
//
end event

event resize;Integer		li_posi_y, li_objeto

//dw_1.Resize(This.WorkSpaceWidth() - 510,This.WorkSpaceHeight() - dw_1.y - 75)
//
//dw_1.x					= 78
//st_encabe.width		= dw_1.width
//
//
//pb_lectura.x			= This.WorkSpaceWidth() - 292
//pb_lectura.y			= 300
//pb_lectura.width		= 233
//pb_lectura.height		= 196
//
//
//pb_nuevo.x				= This.WorkSpaceWidth() - 292
//pb_nuevo.width			= 233
//pb_nuevo.height		= 196
//
//pb_insertar.x			= This.WorkSpaceWidth() - 292
//pb_insertar.width		= 233
//pb_insertar.height	= 196
//
//pb_eliminar.x			= This.WorkSpaceWidth() - 292
//pb_eliminar.width		= 233
//pb_eliminar.height	= 196
//
//pb_grabar.x				= This.WorkSpaceWidth() - 292
//pb_grabar.width		= 233
//pb_grabar.height		= 196
//
//pb_imprimir.x			= This.WorkSpaceWidth() - 292
//pb_imprimir.width		= 233
//pb_imprimir.height	= 196
//

//li_posi_y	= gb_2.y - 92
//
//IF pb_nuevo.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 195
//	pb_nuevo.y	= li_posi_y
//END IF
//
//IF pb_insertar.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 195
//	pb_insertar.y	= li_posi_y
//END IF
//
//IF pb_eliminar.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 195
//	pb_eliminar.y	= li_posi_y
//END IF
//
//IF pb_grabar.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 195
//	pb_grabar.y		= li_posi_y
//END IF
//
//IF pb_imprimir.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 195
//	pb_imprimir.y	= li_posi_y
//END IF
//
//pb_salir.x				= This.WorkSpaceWidth() - 292
//pb_salir.y				= 1200
//pb_salir.width			= 233
//pb_salir.height		= 196
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_modificadetallefruta
integer x = 87
integer y = 60
integer width = 3355
integer height = 416
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_modificadetallefruta
integer x = 3593
integer y = 496
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;dw_3.Enabled			=	TRUE
dw_4.Enabled			=	TRUE
em_numero.Enabled	=	TRUE

dw_3.Reset()
dw_3.InsertRow(0)
dw_3.SetFocus()
dw_3.Object.clie_codigo.Background.Color=	RGB(255, 255, 255)
dw_3.SetItem(1,"clie_codigo", gi_codexport)

dw_4.Reset()
dw_4.InsertRow(0)
dw_4.Object.plde_codigo.Background.Color=	RGB(255, 255, 255)
dw_4.SetItem(1,"plde_codigo", gi_codplanta)

em_numero.BackColor=	RGB(255, 255, 255)
em_numero.text = ""



istr_mant.argumento[1] =String(gi_codexport)
istr_mant.argumento[2] =String(gi_codplanta)
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_modificadetallefruta
integer x = 3593
integer y = 200
integer taborder = 50
end type

event pb_lectura::clicked;integer li_nombrevari


IF IsNull(dw_3.Object.clie_codigo[1]) OR dw_3.Object.clie_codigo[1] = 0 THEN
	MessageBox("Atención","Debe Seleccionar Cliente Previamente",Exclamation!)
	RETURN
ELSEIF IsNull(dw_4.Object.plde_codigo[1]) OR  dw_4.Object.plde_codigo[1] = 0 THEN
	MessageBox("Atención","Debe Seleccionar Planta Previamente",Exclamation!)
	RETURN
ELSEIF IsNull(integer(em_numero.text)) OR integer(em_numero.text) = 0 THEN
	MessageBox("Atención","Debe Seleccionar Nº de Pallet Previamente",Exclamation!)
	RETURN
	em_numero.SetFocus()
ELSE
	dw_3.Enabled	=	FALSE
	dw_4.Enabled	=	FALSE
	em_numero.Enabled	=	FALSE
	dw_3.Object.clie_codigo.Background.Color=	RGB(166,180,210)
	dw_4.Object.plde_codigo.Background.Color=	RGB(166,180,210)
	em_numero.BackColor=	RGB(166,180,210)
	Parent.PostEvent("ue_recuperadatos")
	
END IF	
		
	

		
			
	



end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_modificadetallefruta
boolean visible = false
integer x = 3593
integer y = 856
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_modificadetallefruta
boolean visible = false
integer x = 3593
integer y = 676
integer taborder = 80
end type

event pb_insertar::clicked;//
end event

type pb_salir from w_mant_directo`pb_salir within w_mant_modificadetallefruta
integer x = 3593
integer y = 1600
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_modificadetallefruta
integer x = 3593
integer y = 1216
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_modificadetallefruta
integer x = 3593
integer y = 960
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_modificadetallefruta
integer x = 87
integer y = 496
integer width = 3355
integer height = 1440
integer taborder = 60
string dataobject = "dw_mues_modificadetalle_fruta"
boolean hscrollbar = true
end type

event dw_1::clicked;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1

IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
END IF

	
IF dw_1.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE	
	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nNo existen mas registros"
		ls_colu[li_cont]	= "paen_numero"
	END IF
	
	IF IsNull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0	THEN
	END IF
	
	IF IsNull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0	THEN
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	
		ii_contador = 1
		
		do while ii_contador <= dw_1.RowCount() and dw_1.Object.Paen_numero[ii_contador] <> 0
			ii_contador++
		loop 
		ii_contador --
		dw_1.SetRow(ii_contador)
		Message.DoubleParm = -1
	END IF
END IF

IF dw_1.rowcount() > 0 THEN
	IF dw_1.Object.Paen_numero[il_fila] = 0 THEN
		dw_1.SetRow(ii_contador)
	END IF
END IF



//CHOOSE CASE ls_Columna	
//		
//	CASE "pafr_varrot"
//		istr_mant.Argumento[2]	=	Data	
//			dw_1.SetItem(il_fila,"vari_nombre_1",f_variedadnom(integer(data)))
//			dw_1.SetItem(il_fila,"pafr_calrot",ls_null)
//			IF NoExisteVariedad(ls_Columna,Data)	THEN
//			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
//			RETURN 1
//		END IF
//	
//END CHOOSE
//
//
//
//
//
end event

event dw_1::itemchanged;call super::itemchanged;String ls_Columna, ls_null, ls_varnom, ls_calibre
Integer	li_Nula, li_especie, li_variedad 
Long	ll_productor


SetNull(li_Nula)
SetNull(ls_null)

dw_1.AcceptText()

ls_Columna	=	Dwo.Name

CHOOSE CASE ls_Columna	
		
	CASE "pafr_varrot"
		istr_mant.Argumento[2]	=	Data	
			//dw_1.SetItem(il_fila,"vari_nombre_rotula",f_variedadnom(integer(data)))
			IF NoExisteVariedad(ls_Columna,Data)	THEN
				dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
				RETURN 1
			
			END IF
			
	CASE "vari_codigo"
		istr_mant.Argumento[2]	=	Data	
			//dw_1.SetItem(il_fila,"vari_nombre_rotula",f_variedadnom(integer(data)))
			IF NoExisteVariedad(ls_Columna,Data)	THEN
				dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
				RETURN 1
			
			END IF		
	
	CASE 'emba_codigo'
		IF existeembalaje(Data,dw_3.Object.clie_codigo[1])	THEN
			dw_1.SetItem(il_Fila,'emba_codigo',String(li_Nula))
			RETURN 1
			
		END IF
		
	CASE 'pafr_embrea'
		IF existeembalaje(Data,dw_3.Object.clie_codigo[1])	THEN
			dw_1.SetItem(il_Fila,'pafr_embrea',String(li_Nula))
			RETURN 1
			
		END IF	
		
	CASE "pafr_calibr"
		li_especie 	= dw_1.Object.espe_codigo[row]
		li_variedad = dw_1.Object.vari_codigo[row]	
		
		IF NOT iuo_calibre.existe(li_especie,li_variedad,data,True,sqlca) THEN
			dw_1.SetItem(row, ls_columna, ls_null)
			RETURN 1
		ELSE	
			dw_1.Object.pafr_calibr[row] = iuo_calibre.calibre
		END IF	
			
	CASE "pafr_calrot"
		li_especie 	= dw_1.Object.espe_codigo[row]
		li_variedad = dw_1.Object.vari_codigo[row]	
		
		IF NOT iuo_calibre.existe(li_especie,li_variedad,data,True,sqlca) THEN
			dw_1.SetItem(row, ls_columna, ls_null)
			RETURN 1
		ELSE	
			dw_1.Object.pafr_calrot[row] = iuo_calibre.calibre
		END IF	
			
	CASE "prod_codigo"
			
		IF existeproductor(Long(data)) THEN
			dw_1.SetItem(row, ls_columna, Long(ls_null))
			RETURN 1
		END IF		
		
	CASE "pafr_prdrot"
			
		IF existeproductor(Long(data)) THEN
			dw_1.SetItem(row, ls_columna, Long(ls_null))
			RETURN 1
		END IF	
		
	CASE "pafr_copack"
			
		IF existepacking(Integer(data)) THEN
			dw_1.SetItem(row, ls_columna, Integer(ls_null))
			RETURN 1
		END IF	
		
	CASE "pafr_rotpak"
			
		IF existepacking(Integer(data)) THEN
			dw_1.SetItem(row, ls_columna, Integer(ls_null))
			RETURN 1
		END IF	
	
	CASE "pafr_huert1"
		
		ll_productor = dw_1.Object.prod_codigo[row] 
		IF existepredio(Integer(data),ll_productor) THEN
			dw_1.SetItem(row, ls_columna, Integer(ls_null))
			RETURN 1
		END IF	
		
	CASE "pafr_huert4"
		
		ll_productor = dw_1.Object.prod_codigo[row] 
		IF existepredio(Integer(data),ll_productor) THEN
			dw_1.SetItem(row, ls_columna, Integer(ls_null))
			RETURN 1
		END IF		
	
		
	END CHOOSE	




end event

event dw_1::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.name		
	CASE "buscavariedad"
			buscavariedad()
			
	
END CHOOSE


end event

event dw_1::rowfocuschanged;integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF
end event

event dw_1::dwnkey;This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!
		Message.DoubleParm = 0
		
		Parent.TriggerEvent("ue_validaregistro")
		
		IF Message.DoubleParm = -1 THEN
			This.SetRedraw(True)
			RETURN -1
		ELSEIF Key = KeyDownArrow! AND il_fila = dw_1.RowCount() THEN
			Parent.TriggerEvent("ue_nuevo")
		END IF
		
	CASE KeyTab!
		IF is_ultimacol = This.GetColumnName() AND il_fila = dw_1.RowCount() THEN
			Message.DoubleParm = 0
			
			Parent.TriggerEvent("ue_validaregistro")
			
			IF Message.DoubleParm = -1 THEN
				This.SetRedraw(True)
				RETURN -1
			ELSE
				Parent.TriggerEvent("ue_nuevo")
				
				This.SetFocus()
				This.SetRedraw(True)
				RETURN -1
			END IF
		END IF

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

type st_2 from statictext within w_mant_modificadetallefruta
integer x = 754
integer y = 228
integer width = 347
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_1 from statictext within w_mant_modificadetallefruta
integer x = 754
integer y = 112
integer width = 347
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

type em_numero from editmask within w_mant_modificadetallefruta
integer x = 1179
integer y = 328
integer width = 407
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[6]=em_numero.text

IF existepallet(Long(em_numero.text)) = False THEN
	This.SetFocus()
END IF
end event

type st_3 from statictext within w_mant_modificadetallefruta
integer x = 754
integer y = 344
integer width = 325
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
string text = "Nro. Pallet"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_3 from datawindow within w_mant_modificadetallefruta
integer x = 1179
integer y = 96
integer width = 1234
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null

SetNull(ll_null)

IF iuo_ClienteProd.existe(Integer(Data), True, sqlca) THEN
	istr_mant.argumento[1]	=	String(data)
	
ELSE
	This.SetItem(1, "clie_codigo", Integer(ll_null))

	RETURN 1
END IF
	

end event

event itemerror;Return 1
end event

type dw_4 from datawindow within w_mant_modificadetallefruta
integer x = 1179
integer y = 212
integer width = 969
integer height = 96
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null


SetNull(ll_null)

IF Not iuo_Planta.Existe(Integer(Data), True, sqlca) THEN
	This.SetItem(1, "plde_codigo", Integer(ll_null))

	RETURN 1
ELSE			
	istr_mant.argumento[2]=String(data)
END IF		



end event

event itemerror;Return 1
end event

