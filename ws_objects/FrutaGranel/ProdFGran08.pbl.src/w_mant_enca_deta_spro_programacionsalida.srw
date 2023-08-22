$PBExportHeader$w_mant_enca_deta_spro_programacionsalida.srw
forward
global type w_mant_enca_deta_spro_programacionsalida from w_mant_encab_deta
end type
type pb_insertar from picturebutton within w_mant_enca_deta_spro_programacionsalida
end type
type dw_6 from datawindow within w_mant_enca_deta_spro_programacionsalida
end type
type tab_1 from tab within w_mant_enca_deta_spro_programacionsalida
end type
type tabpage_1 from userobject within tab_1
end type
type dw_dettab1 from uo_dw within tabpage_1
end type
type tabpage_1 from userobject within tab_1
dw_dettab1 dw_dettab1
end type
type tabpage_2 from userobject within tab_1
end type
type dw_dettab2 from uo_dw within tabpage_2
end type
type tabpage_2 from userobject within tab_1
dw_dettab2 dw_dettab2
end type
type tabpage_3 from userobject within tab_1
end type
type dw_dettab3 from uo_dw within tabpage_3
end type
type tabpage_3 from userobject within tab_1
dw_dettab3 dw_dettab3
end type
type tabpage_4 from userobject within tab_1
end type
type dw_dettab4 from uo_dw within tabpage_4
end type
type tabpage_4 from userobject within tab_1
dw_dettab4 dw_dettab4
end type
type tab_1 from tab within w_mant_enca_deta_spro_programacionsalida
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
end type
type dw_encab from datawindow within w_mant_enca_deta_spro_programacionsalida
end type
type cb_copia from commandbutton within w_mant_enca_deta_spro_programacionsalida
end type
type dw_4 from uo_dw within w_mant_enca_deta_spro_programacionsalida
end type
type dw_3 from uo_dw within w_mant_enca_deta_spro_programacionsalida
end type
type dw_5 from uo_dw within w_mant_enca_deta_spro_programacionsalida
end type
type dw_7 from uo_dw within w_mant_enca_deta_spro_programacionsalida
end type
end forward

global type w_mant_enca_deta_spro_programacionsalida from w_mant_encab_deta
boolean visible = false
integer width = 4091
integer height = 2140
string title = "PROGRAMACION PROCESOS"
string menuname = ""
windowstate windowstate = maximized!
event ue_validapassword ( )
pb_insertar pb_insertar
dw_6 dw_6
tab_1 tab_1
dw_encab dw_encab
cb_copia cb_copia
dw_4 dw_4
dw_3 dw_3
dw_5 dw_5
dw_7 dw_7
end type
global w_mant_enca_deta_spro_programacionsalida w_mant_enca_deta_spro_programacionsalida

type variables
uo_Clientesprod				iuo_cliente
uo_plantadesp				iuo_planta
uo_spro_ordenproceso	iuo_proceso
uo_tipopallet				iuo_pallet
uo_varirotulado				iuo_Rotulado
uo_calibresenvases		iuo_CalibresEnv

Integer						il_secuencia

DataWindowChild			idwc_linea, idwc_secuencia, idwc_linea_packing, idwc_variedad,idwc_calibre
DataWindow				dw_detalle

String			is_abcd[] = {"A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"}
end variables

forward prototypes
public function boolean existeprogramacion ()
public subroutine cargaproceso ()
public function boolean cargaembalaje (string as_embacodigo)
public function boolean duplicado (string as_columna, string as_valor, integer ai_fila)
public function boolean existecategoria (integer ai_categoria)
public function integer recupera_secuencia (integer planta, integer tipo, integer numero, integer cliente)
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine cargaclon (boolean ab_abreventana)
public subroutine wf_bloqueacolumnas ()
end prototypes

event ue_validapassword();str_mant	lstr_mant

IF Not IsNull(gstr_paramplanta.passpack) OR gstr_paramplanta.passpack <> '' THEN
	lstr_mant.Argumento[1]	=	"Granel"
	lstr_mant.Argumento[2]	=	gstr_paramplanta.passpack
	
	OpenWithParm(w_password, lstr_mant)
	
	lstr_mant	=	Message.PowerObjectParm
	
	IF lstr_mant.Respuesta = 0 THEN Close(This)
END IF
end event

public function boolean existeprogramacion ();Integer	li_cliente, li_planta, li_tipoorden, li_numeroorden, li_estado
Boolean	lb_retorno	=	False

li_planta				=	dw_2.Object.plde_codigo[1]
li_tipoorden			=	Integer(istr_mant.Argumento[2])
li_cliente				=	dw_2.Object.Clie_codigo[1]
li_numeroorden			=	Integer(istr_mant.Argumento[4])

Select IsNull(Max(prsa_estado),0)
   	into :li_estado
   	from dbo.spro_programasalidas
	where plde_codigo 	=:li_planta
	    and orpr_tipord 	=:li_tipoorden
	    and clie_codigo 	=:li_cliente
	    and orpr_numero	=:li_numeroorden
Using sqlca;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Programación de Salidas")
ELSEIF sqlca.SQLCode <> 100  THEN // li_estado > 0 THEN//
	lb_Retorno	=	True
END IF

RETURN lb_Retorno
end function

public subroutine cargaproceso ();Integer	Respuesta
DO
	IF dw_3.Retrieve(iuo_proceso.planta, 	  iuo_proceso.linea) = -1 OR 	&
		dw_4.Retrieve(iuo_proceso.planta, 	  iuo_proceso.Cliente, 			&
						  iuo_proceso.TipoOrden,  iuo_proceso.FechaOrden, 		&
						  iuo_proceso.NumeroOrden, 6) = -1 OR 						&
		dw_5.Retrieve(iuo_proceso.planta, 	  iuo_proceso.Cliente, 			&
						  iuo_proceso.TipoOrden,  iuo_proceso.FechaOrden, 		&
						  iuo_proceso.NumeroOrden, 7) = -1 THEN
		respuesta 	= 	MessageBox("Error en Base de Datos", "No es posible conectar "	+ &
									  	  "la Base de Datos.", Information!, RetryCancel!)
	ELSE
		pb_imprimir.Enabled				=	True
		pb_ins_det.Enabled				=	True
		pb_eli_det.Enabled				=	True

		dw_1.GetChild("psrd_lincon", idwc_linea)
		idwc_linea.SetTransObject(sqlca)
		idwc_linea.Retrieve(iuo_proceso.planta, iuo_proceso.linea)
		
		dw_1.GetChild('prsd_calrot', idwc_calibre)
		idwc_calibre.SetTransObject(Sqlca)
		If idwc_calibre.Retrieve(iuo_proceso.Especie,0,0) = 0 Then idwc_calibre.InsertRow(0)		
		
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end subroutine

public function boolean cargaembalaje (string as_embacodigo);String	ls_codigo, ls_nombre
Integer	li_tipoen, li_codenvase, li_envacodigo, li_envatipoen, li_clie_codigo

li_clie_codigo = dw_2.Object.Clie_codigo[dw_2.RowCount()]

SELECT	emba_nombre, enva_tipoen, enva_codigo, enva_codigo, enva_tipoen
	INTO 	:ls_nombre, :li_tipoen, :li_codenvase, :li_envacodigo, :li_envatipoen
	FROM	dbo.embalajesprod
	WHERE emba_codigo	= :as_embacodigo
	  AND clie_codigo = :li_clie_codigo;

IF SqlCa.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embalajes")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje (" + ls_codigo + &
					"), no ha sido creado en tabla respectiva.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
END IF


RETURN True
end function

public function boolean duplicado (string as_columna, string as_valor, integer ai_fila);Integer	li_salidas, li_busq, li_catego
String		ls_embalajes, ls_calibres

li_salidas 		= 	dw_1.Object.lisa_codigo[ai_fila]
ls_embalajes	=	dw_1.Object.emba_codigo[ai_fila]
ls_calibres		=	dw_1.Object.prsd_calibr[ai_fila]

CHOOSE CASE as_columna
	CASE "lisa_codigo"
		li_salidas 		=	Integer(as_valor)
		
	CASE "emba_codigo"
		ls_embalajes	=	as_valor
		
	CASE "psrd_calibr"
		ls_calibres 		=	as_valor
		
	CASE "cate_codigo"
		li_catego			=	Integer(as_valor)
		
END CHOOSE

li_busq	=	dw_1.Find(	"lisa_codigo	= "	+ String(li_salidas)	+ " AND " + &
								"cate_codigo	= "	+ String(li_catego)	+ " AND " + &
								"emba_codigo 	= '" 	+ ls_embalajes			+ "' AND " + &
								"prsd_calibr 	= '" 	+ ls_calibres 			+	"'", 1, dw_1.RowCount())
IF li_busq > 0 THEN 
	MessageBox("Protección de Duplicidad", "La Programación ingresada ya existe. ~r~n"+&
					 "Salida 	:" 		+ String(li_salidas, '00') 	+ "~r~n" + &
					 "Embalaje	:" 	+ ls_embalajes				+ "~r~n" + &
					 "Categoria	:"	+ String(li_catego, '00') 	+ "~r~n" + &
					 "Calibre	:" 		+ ls_calibres)
	 RETURN TRUE
ELSE
	RETURN FALSE
END IF
end function

public function boolean existecategoria (integer ai_categoria);Long		ll_Numero
Boolean	lb_Retorno = True

SELECT	cate_codigo
	INTO	:ll_Numero
	FROM	dbo.categorias
  WHERE	cate_codigo	=	:ai_categoria;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Categorias")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode=100 THEN
	MessageBox("Atención","Codigo de Categoria No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function integer recupera_secuencia (integer planta, integer tipo, integer numero, integer cliente);Integer	li_secuencia

SELECT IsNull(max(prsd_secuen), 0)
INTO	:li_secuencia
FROM dbo.spro_programasalidadeta
WHERE dbo.spro_programasalidadeta.plde_codigo = :planta AND  
		dbo.spro_programasalidadeta.orpr_tipord = :tipo AND  
		dbo.spro_programasalidadeta.orpr_numero = :numero AND  
		dbo.spro_programasalidadeta.clie_codigo = :cliente;   
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de spro_programasalidadeta")
	
END IF

Return li_secuencia		

end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.plde_codigo.Protect				=	0 
  	dw_2.Object.orpr_tipord.Protect				=	0 	 
	dw_2.Object.orpr_numero.Protect				=	0 	  
	dw_2.Object.clie_codigo.Protect				=	0 	
	
	dw_2.Object.plde_codigo.Color		=	0
	dw_2.Object.orpr_numero.Color	=	0
	dw_2.Object.clie_codigo.Color		= 	0
	
	dw_2.Object.plde_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.orpr_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color		=	RGB(255,255,255)
ELSE
	dw_2.Object.plde_codigo.Protect				=	1
  	dw_2.Object.orpr_tipord.Protect				=	1 	 
	dw_2.Object.orpr_numero.Protect				=	1 	  
	dw_2.Object.clie_codigo.Protect				=	1 
	
	dw_2.Object.plde_codigo.Color		=	RGB(255,255,255)
	dw_2.Object.orpr_numero.Color	=	RGB(255,255,255)
	dw_2.Object.clie_codigo.Color		=	RGB(255,255,255)
	
	dw_2.Object.plde_codigo.BackGround.Color		=	553648127
	dw_2.Object.orpr_numero.BackGround.Color	=	553648127
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_encab.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_encab.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF dw_encab.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_encab.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine cargaclon (boolean ab_abreventana);DataSTore		lds_detalle
str_Busqueda	lstr_Busq
Integer			li_cliente, li_tipo
Long				ll_planta, ll_numero, ll_programa, ll_filas
Boolean			lb_linea1, lb_linea2

dw_1.SetFilter('')
dw_1.Filter()

lds_detalle					=	Create DataSTore
lds_detalle.DataObject	=	dw_1.DataObject
lds_detalle.SetTransObject(sqlca)
dw_1.Reset()
dw_encab.Reset()

IF ab_abreventana THEN

	OpenWithParm(w_busc_programa_proceso, lstr_busq)
	
	lstr_busq	= Message.PowerObjectParm
		IF UpperBound(lstr_busq.argum) > 1 THEN
			IF lstr_busq.argum[1] <> "" THEN
				li_cliente	=	Integer(lstr_busq.argum[1])
				ll_planta 	= 	Long(lstr_Busq.Argum[2])
				li_tipo 		= 	Integer(lstr_busq.argum[3])
				ll_numero 	= 	Long(lstr_busq.argum[4])
				ll_programa	=	Long(lstr_busq.argum[5])
				
				IF ll_programa <> dw_2.Object.ppre_numero[1] THEN
					MessageBox("Protección de Datos", "No se puede realizar la carga de una orden con otro programa", STopSign!)
					Return
				END IF
			ELSE
				SetNull(li_cliente)
				SetNull(ll_planta)
				SetNull(li_tipo)
				SetNull(ll_numero)
				SetNull(ll_programa)
			END IF
	END IF
ELSE
	li_cliente	=	dw_2.Object.clie_codigo[1]
	ll_planta 	= 	dw_2.Object.plde_codigo[1]
	li_tipo 		= 	dw_2.Object.orpr_tipord[1]
	ll_numero 	= 	dw_2.Object.orpr_numero[1]
	ll_programa	=	dw_2.Object.ppre_numero[1]
	
	DECLARE Maximo PROCEDURE FOR dbo.fgran_maximo_programa  
		@Cliente		=	:li_cliente,   
		@Planta		=	:ll_planta,   
		@Tipo			=	:li_tipo,   
		@Programa	=	:ll_programa
		USING sqlca;
		
	EXECUTE Maximo ;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDaTos(sqlca, "Ejecución de ProcedimienTo ~'dbo.fgran_maximo_programa~'")
		Return
	ELSE
		FETCH Maximo INTO :ll_planta, :li_tipo, :ll_numero, :li_cliente, :ll_programa;
		CLOSE Maximo; 
	END IF 
END IF

IF lds_detalle.Retrieve(ll_Planta,li_Tipo,ll_Numero,li_Cliente) > 0 THEN
	dw_1.Reset()
	dw_encab.Reset()
	dw_2.object.linea1[1]	=	0
	dw_2.object.linea2[1]	=	0
	dw_2.object.linea3[1]	=	0
	dw_2.object.linea4[1]	=	0
	lds_detalle.RowsCopy(1, lds_detalle.RowCount(), Primary!, dw_1, dw_1.RowCount() + 1, Primary!)
	
	FOR ll_filas = 1 To dw_1.RowCount()
		dw_1.Object.clie_codigo[ll_filas] 	=	dw_2.Object.clie_codigo[1]
		dw_1.Object.plde_codigo[ll_filas] 	= 	dw_2.Object.plde_codigo[1]
		dw_1.Object.orpr_tipord[ll_filas]	= 	dw_2.Object.orpr_tipord[1]
		dw_1.Object.orpr_numero[ll_filas]	= 	dw_2.Object.orpr_numero[1]
		dw_1.SetItemStatus(ll_filas, 0, Primary!, NewModIFied!)
	NEXT
END IF

FOR ll_filas =  1 To dw_1.RowCount()	
	IF dw_1.Object.line_codigo[ll_filas] = 1 THEN
		tab_1.tabpage_1.Enabled				=	True
		dw_2.object.linea1[1]				=	1
	ELSEIF dw_1.Object.line_codigo[ll_filas]  = 2 THEN
		tab_1.tabpage_2.Enabled				=	True
		dw_2.object.linea2[1]				=	1
	ELSEIF dw_1.Object.line_codigo[ll_filas]  = 3 THEN
		tab_1.tabpage_3.Enabled				=	True
		dw_2.object.linea3[1]				=	1
	ELSEIF dw_1.Object.line_codigo[ll_filas]  = 4 THEN
		tab_1.tabpage_4.Enabled				=	True
		dw_2.object.linea4[1]				=	1
	END IF
NEXT
end subroutine

public subroutine wf_bloqueacolumnas ();Return
end subroutine

on w_mant_enca_deta_spro_programacionsalida.create
int iCurrent
call super::create
this.pb_insertar=create pb_insertar
this.dw_6=create dw_6
this.tab_1=create tab_1
this.dw_encab=create dw_encab
this.cb_copia=create cb_copia
this.dw_4=create dw_4
this.dw_3=create dw_3
this.dw_5=create dw_5
this.dw_7=create dw_7
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_insertar
this.Control[iCurrent+2]=this.dw_6
this.Control[iCurrent+3]=this.tab_1
this.Control[iCurrent+4]=this.dw_encab
this.Control[iCurrent+5]=this.cb_copia
this.Control[iCurrent+6]=this.dw_4
this.Control[iCurrent+7]=this.dw_3
this.Control[iCurrent+8]=this.dw_5
this.Control[iCurrent+9]=this.dw_7
end on

on w_mant_enca_deta_spro_programacionsalida.destroy
call super::destroy
destroy(this.pb_insertar)
destroy(this.dw_6)
destroy(this.tab_1)
destroy(this.dw_encab)
destroy(this.cb_copia)
destroy(this.dw_4)
destroy(this.dw_3)
destroy(this.dw_5)
destroy(this.dw_7)
end on

event open;x				=	0
y				=	0
This.Height	=	2520
im_menu		=	m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True


iuo_cliente			=	Create uo_Clientesprod
iuo_planta			=	Create uo_plantadesp				
iuo_proceso			=	Create uo_spro_ordenproceso	
iuo_pallet			=	Create uo_tipopallet
iuo_Rotulado		=	Create uo_varirotulado
iuo_CalibresEnv	=	Create uo_calibresenvases

dw_detalle			=	Create Datawindow

dw_detalle.GetChild('vari_codrel', idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
If idwc_variedad.Retrieve(-1, -1) = 0 Then idwc_variedad.InsertRow(0)

dw_1.GetChild('psrd_lincon', idwc_linea)
idwc_linea.SetTransObject(Sqlca)
If idwc_linea.Retrieve(gstr_ParamPlanta.CodigoPlanta, -1) = -1 Then idwc_linea.InsertRow(0)

dw_1.GetChild('prsd_calrot', idwc_calibre)
idwc_calibre.SetTransObject(Sqlca)
If idwc_calibre.Retrieve(-1,-1,-1) = 0 Then idwc_calibre.InsertRow(0)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_encab.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

This.PostEvent("ue_nuevo")

dw_1.ShareData(dw_detalle)

buscar	= "Código Salida:Nlisa_codigo,Descripción Salida:Nlisa_descri"
ordenar	= "Código Salida:lisa_codigo,Descripción Salida:lisa_descri"

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
This.TriggerEvent("ue_validapassword")

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_fila_1
Date		ldt_FechaProc
Long		ll_Numero
String	ls_Nombre, filtro
Integer	li_planta, li_especie, li_cliente


DO
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_mant.Argumento[1]),Integer(istr_mant.Argumento[2]),&
										Long(istr_mant.Argumento[4]),Integer(istr_mant.Argumento[3]))
							  
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
						Information!, RetryCancel!)		
	ELSEIF ll_fila_e < 1 THEN 
		Return
	ELSE
		DO
				dw_1.GetChild('prsd_calrot', idwc_calibre)
				idwc_calibre.SetTransObject(Sqlca)
				idwc_calibre.Retrieve(dw_2.Object.espe_codigo[1],-1,-1)
			
			IF dw_1.Retrieve(iuo_proceso.planta,iuo_proceso.TipoOrden,iuo_proceso.NumeroOrden,iuo_proceso.Cliente) = -1 OR & 
				dw_3.Retrieve(iuo_proceso.planta, -1) = -1 OR &
				dw_4.Retrieve(iuo_proceso.planta, iuo_proceso.Cliente, iuo_proceso.TipoOrden,iuo_proceso.FechaOrden, iuo_proceso.NumeroOrden, 6) = -1 OR &
				dw_5.Retrieve(iuo_proceso.planta, iuo_proceso.Cliente, iuo_proceso.TipoOrden,iuo_proceso.FechaOrden, iuo_proceso.NumeroOrden, 7) = -1 OR &
				dw_encab.Retrieve(Integer(istr_mant.Argumento[1]),Integer(istr_mant.Argumento[2]),Integer(istr_mant.Argumento[3]),Long(istr_mant.Argumento[4])) = -1 THEN
	  
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_imprimir.Enabled	=	True
				pb_ins_det.Enabled	=	True
				pb_eli_det.Enabled	=	True
				pb_insertar.Enabled	=	True
				HabilitaEncab(False)
				//dw_2.Enabled 			= 	False
				
				dw_2.object.linea1[1]		=	0
				dw_2.object.linea2[1]		=	0
				dw_2.object.linea3[1]		=	0
				
				IF dw_1.RowCount() < 1 THEN
					CargaClon(False)
				ELSE
					FOR ll_fila_1 =  1 TO dw_encab.RowCount()
						dw_2.Object.prsa_observ[1] = dw_encab.Object.prsa_observ[ll_fila_1]
						
						CHOOSE CASE dw_encab.Object.prsa_lineaa[ll_fila_1]
							CASE 1
								tab_1.tabpage_1.Enabled			=	True
								dw_2.object.linea1[1]			=	1
								dw_1.SetFilter("line_codigo = 1" )
								dw_1.Filter()
								dw_3.SetFilter("line_codigo = 1" )
								dw_3.Filter()
								dw_3.SetSort("lisa_codigo asc")
								dw_3.Sort() 
								dw_detalle				=	tab_1.tabpage_1.dw_dettab1
								dw_1.ShareData(dw_detalle)
								tab_1.SelectedTab 	= 	1
								
								dw_detalle.GetChild("psrd_lincon", idwc_linea)
								idwc_linea.SetTransObject(sqlca)
								idwc_linea.Retrieve(dw_2.Object.plde_codigo[1], 1)
								
								dw_detalle.GetChild('prsd_calrot', idwc_calibre)
								idwc_calibre.SetTransObject(Sqlca)
								If idwc_calibre.Retrieve(iuo_proceso.Especie,0,0) = 0 Then idwc_calibre.InsertRow(0)									

							CASE 2
								tab_1.tabpage_2.Enabled			=	True
								dw_2.object.linea2[1]			=	1
								IF dw_2.object.linea1[1] = 0 THEN
									dw_1.SetFilter("line_codigo = 2" )
									dw_1.Filter()
									dw_3.SetFilter("line_codigo = 2" )
									dw_3.Filter()
									dw_3.SetSort("lisa_codigo asc")
									dw_3.Sort()
									dw_detalle				=	Tab_1.tabpage_2.dw_dettab2
									dw_1.ShareData(dw_detalle)
									tab_1.SelectedTab 	= 	2
									
									dw_detalle.GetChild("psrd_lincon", idwc_linea)
									idwc_linea.SetTransObject(sqlca)
									idwc_linea.Retrieve(dw_2.Object.plde_codigo[1], 2)
									
									dw_detalle.GetChild('prsd_calrot', idwc_calibre)
									idwc_calibre.SetTransObject(Sqlca)
									idwc_calibre.Retrieve(dw_2.Object.espe_codigo[1],0,0)									
								END IF
								
							CASE 3
								tab_1.tabpage_3.Enabled			=	True
								dw_2.object.linea3[1]			=	1
								IF dw_2.object.linea1[1] = 0 AND &
									dw_2.object.linea2[1] = 0 THEN
									dw_1.SetFilter("line_codigo = 3" )
									dw_1.Filter()
									dw_3.SetFilter("line_codigo = 3" )
									dw_3.Filter()
									dw_3.SetSort("lisa_codigo asc")
									dw_3.Sort()
									dw_detalle				=	Tab_1.tabpage_3.dw_dettab3
									dw_1.ShareData(dw_detalle)
									tab_1.SelectedTab 	= 	3
									
									dw_detalle.GetChild("psrd_lincon", idwc_linea)
									idwc_linea.SetTransObject(sqlca)
									idwc_linea.Retrieve(dw_2.Object.plde_codigo[1], 3)
									
									dw_detalle.GetChild('prsd_calrot', idwc_calibre)
									idwc_calibre.SetTransObject(Sqlca)
									idwc_calibre.Retrieve(dw_2.Object.espe_codigo[1],0,0)
								END IF
								
							CASE 4
								tab_1.tabpage_4.Enabled			=	True
								dw_2.object.linea4[1]			=	1
								IF dw_2.object.linea1[1] = 0 AND &
									dw_2.object.linea2[1] = 0 AND &
									dw_2.object.linea3[1] = 0 THEN
									dw_1.SetFilter("line_codigo = 4" )
									dw_1.Filter()
									dw_3.SetFilter("line_codigo = 4" )
									dw_3.Filter()
									dw_3.SetSort("lisa_codigo asc")
									dw_3.Sort()
									dw_detalle				=	Tab_1.tabpage_4.dw_dettab4
									dw_1.ShareData(dw_detalle)
									tab_1.SelectedTab 	= 	4
									
									dw_detalle.GetChild("psrd_lincon", idwc_linea)
									idwc_linea.SetTransObject(sqlca)
									idwc_linea.Retrieve(dw_2.Object.plde_codigo[1], 4)
									
									dw_detalle.GetChild('prsd_calrot', idwc_calibre)
									idwc_calibre.SetTransObject(Sqlca)
									idwc_calibre.Retrieve(dw_2.Object.espe_codigo[1],0,0)									
								END IF
								
						END CHOOSE
						
					NEXT
				END IF	
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;Integer	li_f1, li_f2, li_f3, f4, li_f5, li_actual, li_acum, lia_salidas[], lia_sallin[], li_busq, li_correl
String		lsa_embalajes[], lsa_calibres[], lsa_descri[], lsa_lado[],LS


For li_f1 = 1 TO dw_3.RowCount()
	If dw_3.IsSelected(li_f1) Then
		li_acum						=	li_acum + 1
		lia_salidas[li_acum] 	= 	dw_3.Object.lisa_codigo[li_f1]
		lia_sallin[li_acum]		=	dw_3.Object.line_codigo[li_f1]
		lsa_descri[li_acum]		=	dw_3.Object.lisa_descri[li_f1]
	End If
Next

If li_acum = 0 Then
	MessageBox("Error", "No ha seleccionado una Salida para programar", StopSign!)
	Return
Else
	li_acum = 0
End If

For li_f1 = 1 TO dw_4.RowCount()
	If dw_4.IsSelected(li_f1) Then
		li_acum						=	li_acum + 1
		lsa_embalajes[li_acum] 	= 	dw_4.Object.tipo[li_f1]
	End If
Next

If li_acum = 0 Then
	MessageBox("Error", "No ha seleccionado un Embalaje para programar", StopSign!)
	Return
Else
	li_acum = 0
End If

For li_f1 = 1 TO dw_5.RowCount()
	If dw_5.IsSelected(li_f1) Then
		li_acum						=	li_acum + 1
		lsa_calibres[li_acum] 	= 	dw_5.Object.tipo[li_f1]
	End If
Next

If li_acum = 0 Then
	MessageBox("Error", "No ha seleccionado un Calibre para programar", StopSign!)
	Return
Else
	li_acum = 0
End If

For li_f1 = 1 TO dw_7.RowCount()
	If dw_7.IsSelected(li_f1) Then
		li_acum					=	li_acum + 1
		lsa_lado[li_acum] 		= 	dw_7.Object.lado[li_f1]
	End If
Next

If li_acum = 0 Then
	lsa_lado[1]	=	'A'
	li_acum 		= 	0
End If


dw_3.SelectRow(0, False)
dw_4.SelectRow(0, False)
dw_5.SelectRow(0, False)
dw_7.SelectRow(0, False)

If dw_1.RowCount() > 0 Then
	li_correl	=	dw_1.Object.prsd_secuen[dw_1.RowCount()]
Else
	li_correl	=	0
End If

If IsNull(il_secuencia) Then il_secuencia = 0

For li_f1 = 1 TO UpperBound(lia_salidas)
	For li_f2 = 1 TO UpperBound(lsa_embalajes)
		For li_f3 = 1 TO UpperBound(lsa_calibres)
			For li_f5 = 1 TO UpperBound(lsa_lado)
				
				
				li_busq	=	dw_1.Find("lisa_codigo = "	+ String(lia_salidas[li_f1], '00') + " AND " +  &
											  "trim(prsd_codsec) = '"  + String(lia_salidas[li_f1], '00') + lsa_lado[li_f5] + "'", &
												1, dw_1.RowCount())/*
											  "emba_codigo = '" 	+ lsa_embalajes[li_f2] + "' AND " + &
											  "prsd_calibr = '" 	+ lsa_calibres[li_f3]+ "' AND " + &*/
				If li_busq > 0 Then 
					MessageBox("Protección de Duplicidad", "La Programación ingresada ya existe. ~r~n"+ &
									 "Salida	:" 	+ String(lia_salidas[li_f1], '00') + "~r~n" + &
									 "Lado	:"	+ String(lia_salidas[li_f1]) 		+ lsa_lado[li_f5] + "~r~n" + &
									 "Embalaje	:"	+ lsa_embalajes[li_f2]			+ "~r~n" + &
									 "Calibre   	:"	+ lsa_calibres[li_f3])
							
				Else
					li_actual 									= 	dw_1.InsertRow(0)
					dw_1.GetChild('prsd_calrot', idwc_calibre)
					idwc_calibre.SetTransObject(Sqlca)
					If idwc_calibre.Retrieve(iuo_proceso.Especie,0,0) = 0 Then idwc_calibre.InsertRow(0)		
					
					dw_1.object.plde_codigo[li_actual]	=	iuo_proceso.Planta
					dw_1.object.orpr_tipord[li_actual]	=	iuo_proceso.TipoOrden
					dw_1.object.orpr_numero[li_actual]	=	iuo_proceso.NumeroOrden
					dw_1.object.clie_codigo[li_actual]	=	iuo_proceso.Cliente
					dw_1.object.espe_codigo[li_actual]	=	iuo_proceso.Especie
					dw_1.object.vari_codigo[li_actual]	=	iuo_proceso.Variedad
					dw_1.object.line_codigo[li_actual]	=	lia_sallin[li_f1]
					dw_1.object.prsd_codsec[li_actual]	=	String(lia_salidas[li_f1], '00') + lsa_lado[li_f5]
					dw_1.Object.lisa_codigo[li_actual]	=	lia_salidas[li_f1]
					dw_1.Object.lisa_descri[li_actual]	=	lsa_descri[li_f1]
					dw_1.Object.emba_codigo[li_actual]	=	lsa_embalajes[li_f2]
					dw_1.Object.prsd_calibr[li_actual]	=	lsa_calibres[li_f3]
					dw_1.Object.prsd_calrot[li_actual]	=	lsa_calibres[li_f3]
					dw_1.Object.merc_codigo[li_actual]	=	99
					dw_1.Object.psrd_lincon[li_actual]	=	lia_salidas[li_f1]
					dw_1.Object.cate_codigo[li_actual]	=	1
				End If
			Next
		Next
	Next
Next

dw_1.SetSort("lisa_codigo asc, emba_codigo asc, prsd_calibr asc")
dw_1.Sort()

pb_grabar.enabled	=	True
pb_imprimir.enabled	=	True
pb_eliminar.enabled	=	True
end event

event ue_nuevo;Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif2	=	dw_2.GetNextModified(0, Primary!)
		
//			IF ib_ModEncab OR dw_1.GetNextModified(0, Primary!) > 0 THEN
			IF dw_1.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_1.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
pb_insertar.Enabled		= 	False
Habilitaencab(True)

dw_3.Reset()
dw_4.Reset()
dw_5.Reset()

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
istr_mant.Argumento[1]			=	String(gstr_ParamPlanta.CodigoPlanta)
dw_2.Object.orpr_tipord[1]		=	4
istr_mant.Argumento[2]			=	String(4)
dw_2.Object.clie_codigo[1]		=	gi_CodExport
istr_mant.Argumento[3]			=	String(gi_CodExport)
dw_2.SetColumn('orpr_numero')

dw_1.SetFilter('')
dw_1.Filter( )

dw_2.SetFocus()

idwc_secuencia.Reset()

dw_7.Reset()
dw_7.InsertRow(0)
dw_7.Object.Lado[1]				=	'A'
dw_7.InsertRow(0)
dw_7.Object.Lado[2]				=	'B'
dw_7.InsertRow(0)
dw_7.Object.Lado[3]				=	'C'

Tab_1.TabPage_1.Enabled			=	False
Tab_1.TabPage_2.Enabled			=	False
Tab_1.TabPage_3.Enabled			=	False
Tab_1.TabPage_4.Enabled			=	False
end event

event ue_borra_detalle;call super::ue_borra_detalle;Integer	li_filas

SetPointer(HourGlass!)

w_main.SetMicroHelp("Validando la eliminación de detalle...")
FOR li_filas	=	1 TO dw_1.RowCount()
	IF dw_1.IsSelected(li_filas) THEN
		IF dw_1.DeleteRow(li_filas) = 1 THEN
			w_main.SetMicroHelp("Borrando Registro...")
			li_filas = li_filas - 1
			SetPointer(Arrow!)
		ELSE
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	END IF
NEXT

dw_1.SelectRow(0, False)

 IF dw_1.RowCount() = 0 THEN 
	pb_eli_det.Enabled = False
END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "INFORME DE PROGRAMACION DE SALIDAS"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_spro_programasalidasdeta"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(iuo_proceso.planta,iuo_proceso.TipoOrden,&
								iuo_proceso.NumeroOrden,iuo_proceso.Cliente)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_antesguardar;Integer	li_filas, li_secue, li_salant, li_linea
String		ls_emba, ls_embant, ls_calant
Boolean	lb_flag

il_secuencia = recupera_secuencia(Integer(istr_mant.Argumento[1]),Integer(istr_mant.Argumento[2]),&
											Integer(istr_mant.Argumento[4]),Integer(istr_mant.Argumento[3]))

For li_filas = 1 To dw_1.RowCount()
	ls_emba	=	dw_1.Object.tpem_codigo[li_filas]
	If Len(ls_emba) < 1 Or IsNull(ls_emba) Then
		dw_1.Object.tpem_codigo[li_filas] = "96"
	End  If
	If dw_1.GetItemStatus(li_filas, 0, Primary!) = NewModified! Or &
		dw_1.GetItemStatus(li_filas, 0, Primary!) = DataModified! Then
			il_Secuencia++
			dw_1.Object.prsd_secuen[li_filas]	=	il_secuencia			
	End If
Next

dw_1.SetFilter("")
dw_1.Filter()

If dw_1.RowCount() < 1 Then
	Message.DoubleParm = -1
	Return
End If

FOR li_linea = 1 TO 4
	If dw_2.GetItemNumber(1, "Linea" + String(li_linea)) = 1 THEN
		li_secue	=	dw_encab.Find("prsa_lineaa = " + String(li_linea), 1, dw_encab.RowCount())
		
		If li_secue = 0 Then
			li_secue												=	dw_encab.InsertRow(0)
			dw_encab.Object.prsa_estado[li_secue]		=	0
			dw_encab.Object.orpr_fecpro[li_secue]		=	iuo_proceso.FechaOrden
			dw_encab.Object.line_codigo[li_secue]		=	li_linea
			dw_encab.Object.prsa_lineaa[li_secue]		=	li_linea
			dw_encab.Object.prsa_observ[li_secue]		=	dw_2.Object.prsa_observ[1]
			dw_encab.Object.clie_codigo[li_secue]		=	dw_2.Object.clie_codigo[1]
			dw_encab.Object.plde_codigo[li_secue]		=	dw_2.Object.plde_codigo[1]
			dw_encab.Object.orpr_numero[li_secue]		=	dw_2.Object.orpr_numero[1]
			dw_encab.Object.orpr_tipord[li_secue]		=	dw_2.Object.orpr_tipord[1]
		Else
			dw_encab.Object.prsa_observ[li_secue]		=	dw_2.Object.prsa_observ[1]
		End If
		
		lb_flag	= 	False
		
		For li_filas = dw_1.RowCount() To 1 Step -1
			If dw_1.Object.line_codigo[li_filas] = li_linea Then
				lb_flag = True
				Exit
			End If
		Next
		
		If Not lb_flag Then
			dw_1.SetFilter("line_codigo = " + String(li_linea) )
			dw_1.Filter()
			dw_3.SetFilter("line_codigo = " + String(li_linea) )
			dw_3.Filter()
			dw_3.SetSort("lisa_codigo asc")
			dw_3.Sort()
			MessageBox("Error", "Linea " + String(li_linea) + " sin salidas programadas," + &
									  "~r~n" + "Programelas o deseleccione la Linea indicada.")
			Message.DoubleParm = -1
			Return
		End If
	Else
		
		li_secue 	=	dw_encab.Find("prsa_lineaa = " + String(li_linea), 1, dw_encab.RowCount())
		If li_secue > 0 Then
			li_secue	=	dw_encab.DeleteRow(li_secue)
		End If
		
		For li_filas = dw_1.RowCount() To 1 Step -1
			If dw_1.Object.line_codigo[li_filas] = li_linea Then
				dw_1.DeleteRow(li_filas)
			End If
		Next
	End If
Next

IF dw_2.object.linea1[1] = 1 Then
	dw_1.SetFilter("line_codigo = 1" )
	dw_1.Filter()
	dw_3.SetFilter("line_codigo = 1" )
	dw_3.Filter()
	dw_3.SetSort("lisa_codigo asc")
	dw_3.Sort()
	dw_detalle				=	tab_1.tabpage_1.dw_dettab1
	dw_1.ShareData(dw_detalle)
	tab_1.SelectedTab 	= 	1
	
ElSEIf dw_2.object.linea2[1] = 1 THEN
	dw_1.SetFilter("line_codigo = 2" )
	dw_1.Filter()
	dw_3.SetFilter("line_codigo = 2" )
	dw_3.Filter()
	dw_3.SetSort("lisa_codigo asc")
	dw_3.Sort()
	dw_detalle				=	Tab_1.tabpage_2.dw_dettab2
	dw_1.ShareData(dw_detalle)
	tab_1.SelectedTab 	= 	2
	
ELSEIf dw_2.object.linea3[1] = 1 THEN
	dw_1.SetFilter("line_codigo = 3" )
	dw_1.Filter()
	dw_3.SetFilter("line_codigo = 3" )
	dw_3.Filter()
	dw_3.SetSort("lisa_codigo asc")
	dw_3.Sort()
	dw_detalle				=	Tab_1.tabpage_3.dw_dettab3
	dw_1.ShareData(dw_detalle)
	tab_1.SelectedTab 	= 	3
	
ELSEIf dw_2.object.linea4[1] = 1 THEN
	dw_1.SetFilter("line_codigo = 4" )
	dw_1.Filter()
	dw_3.SetFilter("line_codigo = 4" )
	dw_3.Filter()
	dw_3.SetSort("lisa_codigo asc")
	dw_3.Sort()
	dw_detalle				=	Tab_1.tabpage_4.dw_dettab4
	dw_1.ShareData(dw_detalle)
	tab_1.SelectedTab 	= 	4
	
END IF
	
dw_1.SetSort("prsd_codsec desc, lisa_codigo asc, emba_codigo asc, prsd_calibr asc")
dw_1.Sort()

dw_1.SetSort("lisa_codigo asc, emba_codigo asc, prsd_calibr asc")
dw_1.Sort()
end event

event closequery; IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			Message.ReturnValue = 1 
		CASE 0
			IF dw_3.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.triggerevent("ue_guardar")
						IF message.doubleparm = -1 THEN Message.ReturnValue = 1
						RETURN
						
					CASE 3
						Message.ReturnValue = 1
						RETURN
						
				END CHOOSE
			END IF
	END CHOOSE
END IF


end event

event ue_seleccion;call super::ue_seleccion;str_Busqueda	lstr_Busq

istr_mant.argumento[1] 	= ''
istr_mant.argumento[2] 	= 	''
istr_mant.argumento[3] 	= 	''
istr_mant.argumento[4] 	= 	''
	
OpenWithParm(w_busc_programa_proceso, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	istr_mant.argumento[1] 	= 	lstr_busq.argum[2]
	istr_mant.argumento[2] 	= 	lstr_Busq.Argum[3]
	istr_mant.argumento[3] 	= 	lstr_busq.argum[1]
	istr_mant.argumento[4] 	= 	lstr_busq.argum[4]
	
	IF Not iuo_proceso.existe(Integer(istr_mant.Argumento[1]), Integer(istr_mant.Argumento[2]),&
		integer(istr_mant.Argumento[4]),True,sqlca,Integer(istr_mant.Argumento[3])) THEN
	END IF
		
	IF ExisteProgramacion() THEN
		TriggerEvent("ue_recuperadatos")
	ELSE
		CargaProceso()
	END IF
	dw_1.SetFocus()
END IF

end event

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

Tab_1.width	=	This.WorkSpaceWidth() - dw_3.Width - 400
maximo		=	Tab_1.width

dw_2.x	= 37 
dw_2.y	= 37
If 37 + Round((maximo - dw_2.width) / 2, 0) > 37 Then	dw_2.x	= 37 + Round((maximo - dw_2.width) / 2, 0)

Tab_1.x					= 37 + Round((maximo - Tab_1.width) / 2, 0)
Tab_1.y					= 64 + dw_2.Height
Tab_1.Height			= This.WorkSpaceHeight() - Tab_1.y - 41

Tab_1.TabPage_1.dw_dettab1.Width	=	Tab_1.TabPage_1.Width 	- 100
Tab_1.TabPage_1.dw_dettab1.Height	=	Tab_1.TabPage_1.Height 	- 100
Tab_1.TabPage_2.dw_dettab2.Width	=	Tab_1.TabPage_2.Width 	- 100
Tab_1.TabPage_2.dw_dettab2.Height	=	Tab_1.TabPage_2.Height 	- 100
Tab_1.TabPage_3.dw_dettab3.Width	=	Tab_1.TabPage_3.Width 	- 100
Tab_1.TabPage_3.dw_dettab3.Height	=	Tab_1.TabPage_3.Height 	- 100
Tab_1.TabPage_4.dw_dettab4.Width	=	Tab_1.TabPage_4.Width 	- 100
Tab_1.TabPage_4.dw_dettab4.Height	=	Tab_1.TabPage_4.Height 	- 100

dw_3.x 					=	Tab_1.width + Tab_1.x + 10
dw_4.x 					=	Tab_1.width + Tab_1.x + 10
dw_5.x 					=	Tab_1.width + Tab_1.x + dw_4.width + 10
dw_7.x 					=	Tab_1.width + Tab_1.x + dw_4.width + dw_5.width + 10

dw_3.y 					=	Tab_1.y
dw_3.Height				=	Round((Tab_1.Height / 2), 0)

dw_4.y 					=	Tab_1.y + dw_3.Height + 5
dw_4.Height				=	dw_3.Height * 1

dw_5.y 					=	Tab_1.y + dw_3.Height + 5
dw_5.Height				=	dw_3.Height * 1

dw_7.y 					=	Tab_1.y + dw_3.Height + 5
dw_7.Height				=	dw_3.Height * 1

cb_copia.y				= pb_eli_det.y + pb_eli_det.height + 30
cb_copia.x				= This.WorkSpaceWidth() - 310
cb_copia.width			= 275


end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

dw_1.SetFilter("")
dw_1.Filter()

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)
IF dw_encab.RowCount() > 0 THEN  dw_encab.RowsMove(1,dw_encab.RowCount(),Primary!,dw_encab,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_borrar = False
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	pb_Eliminar.Enabled	=	True
	
	dw_2.Enabled			=	Not istr_mant.Solo_Consulta
	pb_Eliminar.Enabled	=	Not istr_mant.Solo_Consulta
	pb_Grabar.Enabled		=	Not istr_mant.Solo_Consulta
	pb_ins_det.Enabled	=	Not istr_mant.Solo_Consulta
	pb_eli_det.Enabled	=	Not istr_mant.Solo_Consulta
	
ELSE
	pb_ins_det.Enabled	=	Not istr_mant.Solo_Consulta
	
END IF
end event

type dw_1 from w_mant_encab_deta`dw_1 within w_mant_enca_deta_spro_programacionsalida
boolean visible = false
integer x = 0
integer y = 420
integer width = 2441
integer height = 1172
boolean titlebar = false
string title = "Detalle de Programación de Salidas"
string dataobject = "dw_mant_mues_spro_programasalidasdeta"
boolean hscrollbar = false
boolean resizable = true
end type

event dw_1::clicked;IF row > 0 THEN THIS.SelectRow(Row, NOT THIS.IsSelected(Row))

end event

event dw_1::getfocus;//
end event

event dw_1::rowfocuschanged;//
end event

event dw_1::buttonclicked;call super::buttonclicked;//Integer	li_Nula
//String	ls_Columna, ls_Nula
//Date		ldt_FechaProc, ldt_FechaSistema, ldt_fecha
//str_busqueda lstr_busq
//
//ls_Columna = dwo.Name
//
//SetNull(ls_Nula)
//
//CHOOSE CASE ls_Columna
//	CASE "b_tpem"
//			
//		lstr_busq.argum[1]	=	istr_mant.Argumento[3]
//		lstr_busq.argum[2]	=	This.Object.emba_codigo[row]
//		
//		OpenWithParm(w_busc_tipopallets, lstr_busq)
//		
//		lstr_busq	= Message.PowerObjectParm
//		
//		IF UpperBound(lstr_busq.argum) > 3 THEN
//			IF lstr_busq.argum[3] <> "" THEN
//				This.Object.tpem_codigo[row]	=	lstr_busq.argum[3]
//			END IF
//		ELSE
//			Return -1
//		END IF
//		Return 1
//
//	
//END CHOOSE
end event

event dw_1::itemerror;call super::itemerror;Return -1
end event

event dw_1::itemchanged;call super::itemchanged;//Integer	li_Nula
//String	ls_Columna, ls_Nula
//Date		ldt_FechaProc, ldt_FechaSistema, ldt_fecha
//
//ls_Columna = dwo.Name
//
//SetNull(li_Nula)
//SetNull(ls_Nula)
//
//CHOOSE CASE ls_Columna
//	CASE "tpem_codigo"
//		IF Not iuo_pallet.existe_porembalaje(this.object.clie_codigo[row], this.object.emba_codigo[row], data, true, sqlca) THEN
//			This.SetItem(row,"tpem_codigo",ls_Nula)
//			RETURN 1
//		END IF
//		
//	CASE "psrd_consol"
//		IF data = '1' THEN
//			This.SetItem(row,"psrd_lincon",Integer(ls_Nula))
//		ELSE
//			This.SetItem(row,"psrd_lincon",THIS.Object.lisa_codigo[row])
//		END IF
//		
//	CASE "cate_codigo"
//		IF NOT existecategoria(Integer(data)) THEN
//			This.SetItem(row,"cate_codigo", li_Nula)
//			This.SetFocus()
//			RETURN 1
//		END IF
//		
//	CASE "emba_codigo"
//		IF Not cargaembalaje(data) THEN
//			THIS.object.Emba_codigo[row] = ls_Nula
//			THIS.SetColumn("emba_codigo")
//			THIS.SetFocus()
//			RETURN 1
//		END IF
//		
//	CASE "prsd_calibr"
//		IF Len(data) <> 3 THEN
//			MessageBox("Error", "El Calibre ingresado no cumple standard Rio Blanco.", StopSign!)
//			THIS.object.prsd_calibr[row] = ls_Nula
//			THIS.SetColumn("prsd_calibr")
//			THIS.SetFocus()
//			RETURN 1
//		END IF
//		
//END CHOOSE
//
//IF duplicado(dwo.name, data, row) THEN
//	RETURN 1
//END IF


end event

type dw_2 from w_mant_encab_deta`dw_2 within w_mant_enca_deta_spro_programacionsalida
integer x = 27
integer y = 24
integer width = 3237
integer height = 332
string dataobject = "dw_mant_prog_de_proceso_ordenenca"
end type

event dw_2::itemchanged;call super::itemchanged;Integer	li_Nula
String	ls_Columna, ls_Nula, filtro
Date		ldt_FechaProc, ldt_FechaSistema, ldt_fecha

ls_Columna = dwo.Name
SetNull(li_Nula)
SetNull(ls_Nula)

Choose Case ls_Columna
	Case "plde_codigo"
		If Not iuo_planta.existe(Integer(data), true, sqlca) Then
			This.SetItem(row,"plde_codigo",Integer(ls_Nula))
			Return 1
		End If
		istr_mant.Argumento[1] = data
		
	Case "orpr_tipord"
		istr_mant.Argumento[2] = data
		
	Case "clie_codigo"
		If Not iuo_cliente.existe(Integer(data), true, sqlca) Then
			This.SetItem(row,"clie_codigo",Integer(ls_Nula))
			Return 1
		End If
		istr_mant.Argumento[3] = data		
	
	Case "orpr_numero"
		If Not iuo_proceso.existe(Integer(istr_mant.Argumento[1]), Integer(istr_mant.Argumento[2]), Integer(data),&
										  True,sqlca,Integer(istr_mant.Argumento[3])) Then
			This.SetItem(row,"orpr_numero",Integer(ls_Nula))
			Return 1
		End If
		istr_mant.Argumento[4] = data
		
		dw_detalle.GetChild('vari_codrel', idwc_variedad)
		idwc_variedad.SetTransObject(Sqlca)
		If idwc_variedad.Retrieve(iuo_proceso.Especie, iuo_proceso.Variedad) = 0 Then idwc_variedad.InsertRow(0)
		
		dw_detalle.GetChild('prsd_calrot', idwc_calibre)
		idwc_calibre.SetTransObject(Sqlca)
		If idwc_calibre.Retrieve(iuo_proceso.Especie,-1,-1) = 0 Then idwc_calibre.InsertRow(0)

		If ExisteProgramacion() Then
			Parent.TriggerEvent("ue_recuperadatos")
		Else
			CargaProceso()
		End If
		
	Case "linea1"
		If Data = '1' Then
			tab_1.tabpage_1.Enabled			=	True
			tab_1.tabpage_1.PictureName	=	"\Desarrollo\Bmp\linea1E.bmp"
			tab_1.SelectedTab = 1
		Else
			tab_1.tabpage_1.Enabled			=	False
			If tab_1.tabpage_2.Enabled Then tab_1.SelectedTab = 2
		End If
		
	Case "linea2"
		If Data = '1' Then
			tab_1.tabpage_2.Enabled			=	True
			tab_1.tabpage_2.PictureName	=	"\Desarrollo\Bmp\linea2E.bmp"
			tab_1.SelectedTab = 2
		Else
			tab_1.tabpage_2.Enabled			=	False
			If tab_1.tabpage_1.Enabled Then tab_1.SelectedTab = 1
		End If
		
	Case "linea3"
		If Data = '1' Then
			tab_1.tabpage_3.Enabled			=	True
			tab_1.tabpage_3.PictureName	=	"\Desarrollo\Bmp\linea3E.bmp"
			tab_1.SelectedTab = 3
		Else
			tab_1.tabpage_3.Enabled			=	False
			If tab_1.tabpage_1.Enabled Then tab_1.SelectedTab = 1
			If tab_1.tabpage_2.Enabled Then tab_1.SelectedTab = 2
		End If
		
	Case "linea4"
		If Data = '1' Then
			tab_1.tabpage_4.Enabled			=	True
			tab_1.tabpage_4.PictureName	=	"\Desarrollo\Bmp\linea4E.bmp"
			tab_1.SelectedTab = 4
		Else
			tab_1.tabpage_4.Enabled			=	False
			If tab_1.tabpage_1.Enabled Then tab_1.SelectedTab = 1
			If tab_1.tabpage_2.Enabled Then tab_1.SelectedTab = 2
			If tab_1.tabpage_3.Enabled Then tab_1.SelectedTab = 3
		End If
		
End Choose
end event

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_mant_enca_deta_spro_programacionsalida
integer x = 3630
integer y = 392
end type

event pb_nuevo::clicked;call super::clicked;dw_6.Reset()
end event

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_mant_enca_deta_spro_programacionsalida
integer x = 3634
integer y = 572
end type

type pb_grabar from w_mant_encab_deta`pb_grabar within w_mant_enca_deta_spro_programacionsalida
integer x = 3634
integer y = 752
end type

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_mant_enca_deta_spro_programacionsalida
integer x = 3634
integer y = 932
end type

type pb_salir from w_mant_encab_deta`pb_salir within w_mant_enca_deta_spro_programacionsalida
integer x = 3634
integer y = 1112
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_mant_enca_deta_spro_programacionsalida
integer x = 3634
integer y = 1480
integer weight = 400
fontcharset fontcharset = ansi!
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
end type

event pb_ins_det::clicked;call super::clicked;//dw_2.SetTransObject(sqlca)
//dw_2.GetChild("secuencia", idwc_secuencia)
//idwc_secuencia.SetTransObject(sqlca)
//idwc_secuencia.Retrieve(iuo_proceso.planta,iuo_proceso.TipoOrden, iuo_proceso.NumeroOrden,iuo_proceso.Cliente)
//								
//il_secuencia = recupera_secuencia(Integer(istr_mant.Argumento[1]),Integer(istr_mant.Argumento[2]),Integer(istr_mant.Argumento[4]),Integer(istr_mant.Argumento[3]))
//						
//Parent.TriggerEvent("ue_nuevo_detalle")
end event

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_mant_enca_deta_spro_programacionsalida
integer x = 3634
integer y = 1796
end type

type pb_buscar from w_mant_encab_deta`pb_buscar within w_mant_enca_deta_spro_programacionsalida
integer x = 3630
integer y = 212
end type

event pb_buscar::clicked;Parent.TriggerEvent("ue_nuevo")
dw_6.Reset()
Parent.TriggerEvent("ue_seleccion")
end event

type pb_insertar from picturebutton within w_mant_enca_deta_spro_programacionsalida
boolean visible = false
integer x = 3634
integer y = 1636
integer width = 302
integer height = 244
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Signo Mas.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Signo Mas-bn.png"
alignment htextalign = left!
long backcolor = 33543637
end type

event clicked;Long ll_fila, ll_new

FOR ll_fila = 1 TO dw_1.RowCount()
	ll_new = dw_6.InsertRow(0)
	
	dw_6.Object.plde_codigo[ll_new] = dw_1.Object.plde_codigo[ll_fila]
	dw_6.Object.orpr_tipord[ll_new] = dw_1.Object.orpr_tipord[ll_fila]
	dw_6.Object.orpr_numero[ll_new] = dw_1.Object.orpr_numero[ll_fila]
	dw_6.Object.clie_codigo[ll_new] = dw_1.Object.clie_codigo[ll_fila]
	dw_6.Object.line_codigo[ll_new] = dw_1.Object.line_codigo[ll_fila]
	dw_6.Object.lisa_codigo[ll_new] = dw_1.Object.lisa_codigo[ll_fila]
	dw_6.Object.prsd_secuen[ll_new] = dw_1.Object.prsd_secuen[ll_fila]
	dw_6.Object.emba_codigo[ll_new] = dw_1.Object.emba_codigo[ll_fila]
	dw_6.Object.prsd_calibr[ll_new] = dw_1.Object.prsd_calibr[ll_fila]
	dw_6.Object.tpem_codigo[ll_new] = dw_1.Object.tpem_codigo[ll_fila] 
	dw_6.Object.psrd_lincon[ll_new] = dw_1.Object.psrd_lincon[ll_fila]
	dw_6.Object.psrd_consol[ll_new] = dw_1.Object.psrd_consol[ll_fila]
	dw_6.Object.cate_codigo[ll_new] = dw_1.Object.cate_codigo[ll_fila]
NEXT

dw_1.Reset()
dw_1.SetFilter('')
dw_1.Filter( )

il_secuencia = recupera_secuencia(Integer(istr_mant.Argumento[1]),Integer(istr_mant.Argumento[2]),&
						Integer(istr_mant.Argumento[4]),Integer(istr_mant.Argumento[3]))
						
FOR ll_fila = 1 TO dw_6.RowCount()
	ll_new = dw_1.InsertRow(0)
	
	dw_1.Object.plde_codigo[ll_new] = dw_6.Object.plde_codigo[ll_fila]
	dw_1.Object.orpr_tipord[ll_new] = dw_6.Object.orpr_tipord[ll_fila]
	dw_1.Object.orpr_numero[ll_new] = dw_6.Object.orpr_numero[ll_fila]
	dw_1.Object.clie_codigo[ll_new] = dw_6.Object.clie_codigo[ll_fila]
	dw_1.Object.line_codigo[ll_new] = dw_6.Object.line_codigo[ll_fila]
	dw_1.Object.lisa_codigo[ll_new] = dw_6.Object.lisa_codigo[ll_fila]
	dw_1.Object.prsd_secuen[ll_new] = il_secuencia + 1//dw_6.Object.prsd_secuen[ll_fila]
	dw_1.Object.emba_codigo[ll_new] = dw_6.Object.emba_codigo[ll_fila]
	dw_1.Object.prsd_calibr[ll_new] = dw_6.Object.prsd_calibr[ll_fila]
	dw_1.Object.tpem_codigo[ll_new] = dw_6.Object.tpem_codigo[ll_fila] 
	dw_1.Object.psrd_lincon[ll_new] = dw_6.Object.psrd_lincon[ll_fila]
	dw_1.Object.psrd_consol[ll_new] = dw_6.Object.psrd_consol[ll_fila]
	dw_1.Object.cate_codigo[ll_new] = dw_6.Object.cate_codigo[ll_fila]
NEXT
end event

type dw_6 from datawindow within w_mant_enca_deta_spro_programacionsalida
boolean visible = false
integer x = 110
integer y = 1608
integer width = 2021
integer height = 792
integer taborder = 100
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_mues_spro_programasalidasdeta"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
boolean livescroll = true
end type

type tab_1 from tab within w_mant_enca_deta_spro_programacionsalida
integer x = 82
integer y = 420
integer width = 2318
integer height = 1488
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean raggedright = true
boolean focusonbuttondown = true
boolean powertips = true
boolean showpicture = false
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
end type

on tab_1.create
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.tabpage_3=create tabpage_3
this.tabpage_4=create tabpage_4
this.Control[]={this.tabpage_1,&
this.tabpage_2,&
this.tabpage_3,&
this.tabpage_4}
end on

on tab_1.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_2)
destroy(this.tabpage_3)
destroy(this.tabpage_4)
end on

event selectionchanged;CHOOSE CASE NewIndex
	CASE 1
		dw_1.SetFilter("line_codigo = 1" )
		dw_1.Filter()
		dw_3.SetFilter("line_codigo = 1" )
		dw_3.Filter()
		dw_3.SetSort("lisa_codigo asc")
		dw_3.Sort()
		dw_detalle	=	This.tabpage_1.dw_dettab1
		dw_1.ShareData(dw_detalle)
		dw_detalle.GetChild("psrd_lincon", idwc_linea)
		idwc_linea.SetTransObject(sqlca)
		idwc_linea.Retrieve(dw_2.Object.plde_codigo[1], 1)	
		
	CASE 2
		dw_1.SetFilter("line_codigo = 2" )
		dw_1.Filter()
		dw_3.SetFilter("line_codigo = 2" )
		dw_3.Filter()
		dw_3.SetSort("lisa_codigo asc")
		dw_3.Sort()
		dw_detalle	=	This.tabpage_2.dw_dettab2
		dw_1.ShareData(dw_detalle)
		dw_detalle.GetChild("psrd_lincon", idwc_linea)
		idwc_linea.SetTransObject(sqlca)
		idwc_linea.Retrieve(dw_2.Object.plde_codigo[1], 2)	
		
	CASE 3
		dw_1.SetFilter("line_codigo = 3" )
		dw_1.Filter()
		dw_3.SetFilter("line_codigo = 3" )
		dw_3.Filter()
		dw_3.SetSort("lisa_codigo asc")
		dw_3.Sort()
		dw_detalle	=	This.tabpage_3.dw_dettab3
		dw_1.ShareData(dw_detalle)
		
		dw_detalle.GetChild("psrd_lincon", idwc_linea)
		idwc_linea.SetTransObject(sqlca)
		idwc_linea.Retrieve(dw_2.Object.plde_codigo[1], 3)
		
		dw_1.GetChild('prsd_calrot', idwc_calibre)
		idwc_calibre.SetTransObject(Sqlca)
		If idwc_calibre.Retrieve(iuo_proceso.Especie,-1, -1) = 0 Then idwc_calibre.InsertRow(0)		
		
	CASE 4
		dw_1.SetFilter("line_codigo = 4" )
		dw_1.Filter()
		dw_3.SetFilter("line_codigo = 4" )
		dw_3.Filter()
		dw_3.SetSort("lisa_codigo asc")
		dw_3.Sort()
		dw_detalle	=	This.tabpage_4.dw_dettab4
		dw_1.ShareData(dw_detalle)
		dw_detalle.GetChild("psrd_lincon", idwc_linea)
		idwc_linea.SetTransObject(sqlca)
		idwc_linea.Retrieve(dw_2.Object.plde_codigo[1], 4)
		
END CHOOSE

dw_detalle.GetChild('vari_codrel', idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
If idwc_variedad.Retrieve(iuo_Proceso.Especie, iuo_Proceso.Variedad) = 0 Then idwc_variedad.InsertRow(0)
		
dw_detalle.GetChild('prsd_calrot', idwc_calibre)
idwc_calibre.SetTransObject(Sqlca)
If idwc_calibre.Retrieve(iuo_proceso.Especie,-1,-1) = 0 Then idwc_calibre.InsertRow(0)		
end event

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 2281
integer height = 1360
boolean enabled = false
long backcolor = 16777215
string text = "Linea 1"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "\Desarrollo 12\Imagenes\Botones\linea1.bmp"
long picturemaskcolor = 536870912
string powertiptext = "Programación Linea 1"
dw_dettab1 dw_dettab1
end type

on tabpage_1.create
this.dw_dettab1=create dw_dettab1
this.Control[]={this.dw_dettab1}
end on

on tabpage_1.destroy
destroy(this.dw_dettab1)
end on

type dw_dettab1 from uo_dw within tabpage_1
integer x = 50
integer y = 32
integer width = 2185
integer height = 1232
integer taborder = 11
string dataobject = "dw_mant_mues_spro_programasalidasdeta"
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
	dw_1.SelectRow(0,False)
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event itemchanged;call super::itemchanged;String		ls_Columna, ls_Nula

ls_Columna = dwo.Name
SetNull(ls_Nula)

Choose Case ls_Columna
	Case "vari_codrel"
		IF Not iuo_Rotulado.existe(iuo_Proceso.Especie, iuo_Proceso.Variedad, Integer(data), True, sqlca) Then
			This.SetItem(row,ls_Columna, Integer(ls_Nula))
			Return 1
		End If
		
	Case "prsd_calrot"
		IF Not iuo_CalibresEnv.Existe(iuo_Proceso.Especie,0,String(data),sqlca) Then
			This.SetItem(row,ls_columna,ls_Nula)
			Return 1
		End If
End Choose
end event

event itemerror;call super::itemerror;Return 1
end event

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 2281
integer height = 1360
boolean enabled = false
long backcolor = 16777215
string text = "Linea 2"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "\Desarrollo 12\Imagenes\Botones\linea2.bmp"
long picturemaskcolor = 536870912
string powertiptext = "Programación Linea 2"
dw_dettab2 dw_dettab2
end type

on tabpage_2.create
this.dw_dettab2=create dw_dettab2
this.Control[]={this.dw_dettab2}
end on

on tabpage_2.destroy
destroy(this.dw_dettab2)
end on

type dw_dettab2 from uo_dw within tabpage_2
integer x = 50
integer y = 32
integer width = 2185
integer height = 1232
integer taborder = 21
string dataobject = "dw_mant_mues_spro_programasalidasdeta"
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
	dw_1.SelectRow(0,False)
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event itemchanged;call super::itemchanged;String		ls_Columna, ls_Nula

ls_Columna = dwo.Name
SetNull(ls_Nula)

Choose Case ls_Columna
	Case "vari_codrel"
		IF Not iuo_Rotulado.existe(iuo_Proceso.Especie, iuo_Proceso.Variedad, Integer(data), True, sqlca) Then
			This.SetItem(row,ls_Columna, Integer(ls_Nula))
			Return 1
		End If

	Case "prsd_calrot"
		IF Not iuo_CalibresEnv.Existe(iuo_Proceso.Especie,0,String(data),sqlca) Then
			This.SetItem(row,ls_columna,ls_Nula)
			Return 1
		End If
		
End Choose
end event

event itemerror;call super::itemerror;Return 1
end event

type tabpage_3 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 2281
integer height = 1360
boolean enabled = false
long backcolor = 16777215
string text = "Linea 3"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "\Desarrollo 12\Imagenes\Botones\linea3.bmp"
long picturemaskcolor = 536870912
dw_dettab3 dw_dettab3
end type

on tabpage_3.create
this.dw_dettab3=create dw_dettab3
this.Control[]={this.dw_dettab3}
end on

on tabpage_3.destroy
destroy(this.dw_dettab3)
end on

type dw_dettab3 from uo_dw within tabpage_3
integer x = 50
integer y = 32
integer width = 2185
integer height = 1232
integer taborder = 31
string dataobject = "dw_mant_mues_spro_programasalidasdeta"
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
	dw_1.SelectRow(0,False)
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event itemerror;call super::itemerror;Return 1
end event

event itemchanged;call super::itemchanged;String		ls_Columna, ls_Nula

ls_Columna = dwo.Name
SetNull(ls_Nula)

Choose Case ls_Columna
	Case "vari_codrel"
		IF Not iuo_Rotulado.existe(iuo_Proceso.Especie, iuo_Proceso.Variedad, Integer(data), True, sqlca) Then
			This.SetItem(row,ls_Columna, Integer(ls_Nula))
			Return 1
		End If

	Case "prsd_calrot"
		IF Not iuo_CalibresEnv.Existe(iuo_Proceso.Especie,0,String(data),sqlca) Then
			This.SetItem(row,ls_columna,ls_Nula)
			Return 1
		End If
		
End Choose
end event

type tabpage_4 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 2281
integer height = 1360
boolean enabled = false
long backcolor = 16777215
string text = "Linea 4"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "\Desarrollo 12\Imagenes\Botones\linea4.bmp"
long picturemaskcolor = 536870912
dw_dettab4 dw_dettab4
end type

on tabpage_4.create
this.dw_dettab4=create dw_dettab4
this.Control[]={this.dw_dettab4}
end on

on tabpage_4.destroy
destroy(this.dw_dettab4)
end on

type dw_dettab4 from uo_dw within tabpage_4
integer x = 50
integer y = 32
integer width = 2185
integer height = 1232
integer taborder = 21
string dataobject = "dw_mant_mues_spro_programasalidasdeta"
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
	dw_1.SelectRow(0,False)
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event itemerror;call super::itemerror;Return 1
end event

event itemchanged;call super::itemchanged;String		ls_Columna, ls_Nula

ls_Columna = dwo.Name
SetNull(ls_Nula)

Choose Case ls_Columna
	Case "vari_codrel"
		IF Not iuo_Rotulado.existe(iuo_Proceso.Especie, iuo_Proceso.Variedad, Integer(data), True, sqlca) Then
			This.SetItem(row,ls_Columna, Integer(ls_Nula))
			Return 1
		End If

	Case "prsd_calrot"
		IF Not iuo_CalibresEnv.Existe(iuo_Proceso.Especie,0,String(data),sqlca) Then
			This.SetItem(row,ls_columna,ls_Nula)
			Return 1
		End If
		
End Choose
end event

type dw_encab from datawindow within w_mant_enca_deta_spro_programacionsalida
boolean visible = false
integer width = 421
integer height = 304
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_spro_spro_programasalidas"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cb_copia from commandbutton within w_mant_enca_deta_spro_programacionsalida
integer x = 3570
integer y = 16
integer width = 274
integer height = 112
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Copia"
end type

event clicked;CargaClon(True)
end event

type dw_4 from uo_dw within w_mant_enca_deta_spro_programacionsalida
integer x = 2414
integer y = 1252
integer width = 416
integer height = 664
integer taborder = 20
boolean titlebar = true
string title = "Embalajes"
string dataobject = "dw_mues_embalajes_etiq_salidas"
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type dw_3 from uo_dw within w_mant_enca_deta_spro_programacionsalida
integer x = 2414
integer y = 444
integer width = 1111
integer height = 820
integer taborder = 11
boolean titlebar = true
string title = "Salidas"
string dataobject = "dw_mues_salidalineapacking_salidas"
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event ue_nomover;call super::ue_nomover;uint wParam, lParam

wParam = Message.WordParm

Choose Case wParam
	Case 61456, 61458
	     Message.Processed = True
	     Message.ReturnValue = 0

End Choose
end event

type dw_5 from uo_dw within w_mant_enca_deta_spro_programacionsalida
integer x = 2830
integer y = 1252
integer width = 366
integer height = 664
integer taborder = 11
boolean titlebar = true
string title = "Calibres"
string dataobject = "dw_mues_calibres_etiq_salidas"
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type dw_7 from uo_dw within w_mant_enca_deta_spro_programacionsalida
integer x = 3195
integer y = 1252
integer width = 329
integer height = 664
integer taborder = 11
boolean titlebar = true
string title = "Lado"
string dataobject = "dw_selecciona_lado"
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

