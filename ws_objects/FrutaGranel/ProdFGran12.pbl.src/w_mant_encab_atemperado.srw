$PBExportHeader$w_mant_encab_atemperado.srw
forward
global type w_mant_encab_atemperado from w_mant_encab_deta_csd
end type
end forward

global type w_mant_encab_atemperado from w_mant_encab_deta_csd
integer width = 4750
integer height = 2360
string title = "MANTENCION DE ATEMPERADO DE LOTES"
string menuname = ""
boolean center = true
event ue_validapassword ( )
end type
global w_mant_encab_atemperado w_mant_encab_atemperado

type variables
w_mant_lotesfrutagranel_atemper	iw_mantencion

DataWindowChild						idwc_Transp,idwc_Camion,idwc_Variedad,idwc_Predio,&
											idwc_Camara,idwc_especie,idwc_especiedet,idwc_especiedet1,idwc_especiedet2,&
											idwc_planta,idwc_plantadw4,idwc_plancodw4, idwc_plantadw1,idwc_Cliente, &
											idwc_ccev_codigo

DateTime									idt_fechasistema
Integer									il_bloqueo
end variables

forward prototypes
public function boolean noexistecliente (integer cliente)
public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, long al_numero, integer ai_cliente)
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
end prototypes

event ue_validapassword();str_mant	lstr_mant
Integer	li_fila

lstr_mant.Argumento[1]	=	"Granel"
lstr_mant.Argumento[2]	=	gstr_paramplanta.Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN
	il_bloqueo	=	1
	
ELSE
	il_bloqueo	=	0
	
END IF
end event

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 	clie_nombre
INTO		:ls_nombre
FROM 		dbo.clientesprod
WHERE		clie_codigo =:cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

Return False
end function

public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, long al_numero, integer ai_cliente);Long		ll_Numero
Boolean	lb_Retorno = True

SELECT mfge_numero
  INTO :ll_Numero
  FROM dbo.spro_movtofrutagranenca
 WHERE plde_codigo	=	:ai_Planta 
   AND tpmv_codigo	=	:ai_TipoMovto
   AND mfge_numero	=	:al_Numero
   AND clie_codigo 	=  :ai_Cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Recepción Fruta Granel")
	lb_Retorno	=	False
	
ELSEIF sqlca.SQLCode = 0 THEN
	istr_mant.Argumento[3]	=	String(al_Numero)
	This.TriggerEvent("ue_recuperadatos")
	
ELSE
	MessageBox("Atención","Número de Correlativo No ha sido generado o ~r~nNo es una " + Lower(This.Title) + ". Ingrese Otro.")
	lb_Retorno	=	False
	
END IF

RETURN lb_Retorno
end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect		=	0
	dw_2.Object.mfge_numero.Protect	=	0
	dw_2.Object.clie_codigo.Color			=	0
	dw_2.Object.mfge_numero.Color		=	0
	dw_2.Object.clie_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.mfge_numero.BackGround.Color	=	RGB(255,255,255)
ELSE
	dw_2.Object.clie_codigo.Protect		=	1
	dw_2.Object.mfge_numero.Protect	=	1
	dw_2.Object.clie_codigo.Color			=	RGB(255,255,255)
	dw_2.Object.mfge_numero.Color		=	RGB(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
	dw_2.Object.mfge_numero.BackGround.Color	=	553648127
END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 THEN
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
		
	ELSE
		lb_Retorno	=	True
		dw_1.ResetUpdate()
		
	END IF
ELSE
	F_ErrorBaseDatos(sqlca, This.Title)
	RollBack;
	
END IF
	
sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

on w_mant_encab_atemperado.create
call super::create
end on

on w_mant_encab_atemperado.destroy
call super::destroy
end on

event open;Integer	li_resultado
String	ls_parametros, ls_nombre
/* Argumentos
istr_mant.argumento[1] = Código Planta
istr_mant.argumento[2] = Tipo de Movimiento
istr_mant.argumento[3] = Número de Despacho
istr_mant.argumento[4] = Sentido del Movimiento de Envases
istr_mant.argumento[5] = Especie
istr_mant.argumento[6] = Productor no se asigna / va en el detalle del lote
istr_mant.argumento[7] = Ultimo Lote Ocupado
istr_mant.argumento[8] = decimales especie ( 0 - 2 ) 
istr_mant.argumento[9] = Parametro para definir si es [M]antencion o [R]ecepcion
istr_mant.argumento[10] = Código Cliente
*/
x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_2.Object.oproceso.visible 			= 	False
dw_2.Object.defg_docrel.visible 		= 	False
dw_2.Object.ordenproceso.visible 	= 	False

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

istr_mant.argumento[1]					=	String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[2]					=	Message.StringParm
istr_mant.argumento[4]					=	'1'
istr_mant.argumento[5]					=	String(gstr_ParamPlanta.CodigoEspecie)
istr_mant.argumento[7]					=	'0'
istr_mant.argumento[9] 					=	'R'
istr_mant.argumento[10] 				=	String(gi_codexport)

IF istr_mant.argumento[2] = '1' THEN
	This.Title	=	This.Title + ' DE HUERTO'
ELSEif istr_mant.argumento[2] = '8' THEN
	This.Title	=	This.Title + ' DE PRE-PROCESO'
END IF

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve() 

dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
ELSE
	idwc_especie.SetSort("espe_nombre A")
	idwc_especie.Sort()
END IF

dw_2.GetChild("tran_codigo", idwc_Transp)
idwc_Transp.SetTransObject(sqlca)

IF idwc_Transp.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Transportistas")
	idwc_Transp.InsertRow(0)
ELSE
	idwc_Transp.SetSort("tran_nombre A")
	idwc_Transp.Sort()	
END IF

dw_1.GetChild("cama_codigo",idwc_Camara)
idwc_Camara.SetTransObject(SQLCA)
idwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_1.GetChild("lote_espcod", idwc_especiedet2)
idwc_especiedet2.SetTransObject(sqlca)
IF idwc_especiedet2.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especiedet2.InsertRow(0)
ELSE
	idwc_especiedet2.SetSort("espe_nombre A")
	idwc_especiedet2.Sort()
END IF

dw_1.GetChild("lote_pltcod",idwc_plantadw1)
idwc_plantadw1.SetTransObject(SQLCA)
idwc_plantadw1.Retrieve()

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.solo_consulta			=	False

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso,  This.Title, "Acceso a Aplicación", 1)
end event

event ue_modifica_detalle;call super::ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	
	istr_mant.dw	=	dw_1
	
	istr_mant.Argumento[07]	=	String(dw_1.Object.lote_codigo[il_fila])
	istr_mant.Argumento[30]	=	String(il_bloqueo)
	OpenWithParm(iw_mantencion, istr_mant)
		
END IF
end event

event ue_nuevo;call super::ue_nuevo;Time		lt_hora

dw_2.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]		=	Integer(istr_mant.argumento[2])

dw_2.Object.espe_codigo[1]		=	gstr_ParamPlanta.CodigoEspecie
dw_2.Object.clie_codigo[1]		=	gi_CodExport

idt_FechaSistema					=	F_FechaHora()

lt_hora								=	Time(Mid(String(idt_FechaSistema, 'dd/mm/yyyy hh:mm:ss'),12,8))

dw_2.Object.mfge_fecmov[1]		=	Date(String(idt_FechaSistema,'dd/mm/yyyy'))
dw_2.Object.refg_horaen[1]		=	lt_hora

dw_2.Object.destare.Visible	=	False

istr_Mant.Argumento[3]			=	''
istr_mant.Argumento[5]			=	String(dw_2.Object.espe_codigo[1])
istr_mant.Argumento[7]			=	'0'

dw_2.SetColumn("mfge_numero")
dw_2.SetFocus()

HabilitaEncab(True)

end event

event ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, ll_fila_env
Boolean lb_cargo = TRUE

w_main.SetMicroHelp("Recuperando Datos...")
SetPointer(HourGlass!)

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Long(istr_mant.argumento[1]),&
										 Long(istr_mant.argumento[2]),&
										 Long(istr_mant.argumento[3]),&
										 Integer(istr_mant.argumento[10]))

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila_e > 0 THEN
		DO
			dw_1.GetChild("vari_codigo", idwc_Variedad)
			idwc_Variedad.SetTransObject(SQLCa)
			idwc_Variedad.Retrieve(dw_2.Object.espe_codigo[1])
			
			ll_fila_d	= dw_1.Retrieve(Long(istr_mant.argumento[1]),&
												 Long(istr_mant.argumento[2]),&
												 Long(istr_mant.argumento[3]),&
												 Integer(istr_mant.argumento[10]))
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
			ELSEIF ll_fila_d = 0 THEN
				MessageBox("Error", "La recepción seleccionada no tiene asociados lotes para "	+	&
										  "atemperado. ~r~nPor favor, de aviso al encargado del "		+	&
										  "sistema si esto no esta correcto", Exclamation!)
				TriggerEvent("ue_nuevo")
				Return
			ELSE
				HabilitaEncab(False)
				PostEvent("ue_listo")
				il_bloqueo	=	1
				IF MessageBox("Confirmación", "¿Desea habilitar la modificación general?", Question!, YesNo!, 2) = 1 THEN
					TriggerEvent("ue_validapassword")
				END IF
				
			END IF
			
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
		
	END IF
	dw_2.SetRedraw(True)

LOOP WHILE respuesta = 1
IF respuesta = 2 THEN Close(This)
			
			
end event

event ue_seleccion;call super::ue_seleccion;IF ib_ok = False THEN RETURN

Str_Busqueda	lstr_busq

lstr_busq.argum[1] = String(gstr_ParamPlanta.CodigoPlanta)
lstr_busq.argum[2] = '1'												// Movimiento de Recepción
lstr_busq.argum[3] = ''													// Cualquier estado
lstr_busq.argum[4] = String(idt_FechaSistema)   				// Desde Fecha de Inicio Ducha
lstr_busq.argum[5] = istr_mant.argumento[5]     				// Especie
lstr_busq.argum[10] = istr_mant.argumento[10]   				// Codigo Cliente
lstr_busq.argum[9] = '7'												//Tipo Frío Atemperado

OpenWithParm(w_busc_spro_movtofrutagranenca_desverd, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	istr_mant.Argumento[1]	=	lstr_busq.argum[1]
	istr_mant.Argumento[2]	=	lstr_busq.argum[2]
	istr_mant.argumento[3]	=	lstr_busq.argum[3]
	istr_mant.argumento[10]	=	lstr_busq.argum[10]
	
	This.TriggerEvent("ue_recuperadatos")

END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
Date		ld_FechaRecepcion


istr_info.titulo	= "INFORME DESVERDIZADO DE LOTES "
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_spro_lotesfrutagranel_desverd"

vinf.dw_1.SetTransObject(sqlca)

ld_FechaRecepcion	=	dw_2.object.mfge_fecmov[1]

fila = vinf.dw_1.Retrieve(Long(istr_mant.argumento[1]),&
								  Long(istr_mant.argumento[2]),&
								  Long(istr_mant.argumento[3]),&
								  Integer(istr_mant.argumento[10]), &
								  ld_FechaRecepcion, ld_FechaRecepcion)
IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.GetChild("ccev_codigo", idwc_ccev_codigo)
	idwc_ccev_codigo.SetTransObject(sqlca)
	idwc_ccev_codigo.Retrieve(dw_2.Object.espe_codigo[1])
	IF gs_Ambiente <> 'Windows' THEN
	   	F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
  	END IF
END IF

SetPointer(Arrow!)
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_mant_encab_atemperado
integer x = 50
integer y = 1280
integer width = 4027
integer height = 852
string title = "Detalle de Lotes Recepcionados a Atemperado"
string dataobject = "dw_mues_spro_lotesfrutagranel_atemper"
end type

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_mant_encab_atemperado
integer x = 41
integer y = 0
integer width = 3333
integer height = 1096
boolean titlebar = true
string dataobject = "dw_mant_movtofrutagranenca_desv"
end type

event dw_2::itemchanged;call super::itemchanged;String	ls_Nula
String	ls_Columna
Date		ld_Fecha
Time		lt_Hora
 

ls_Columna = dwo.Name
SetNull(ls_Nula)
 
CHOOSE CASE ls_Columna
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(row,"clie_codigo",Integer(ls_Nula))
			RETURN 1
		END IF	
	
	CASE "mfge_numero"
		IF NOT ExisteRecepcion(gstr_ParamPlanta.CodigoPlanta, Integer(istr_mant.argumento[2]), Long(data),Integer(istr_mant.argumento[10])) THEN
			This.SetItem(row,"mfge_numero",Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_mant_encab_atemperado
integer x = 4197
integer y = 376
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_mant_encab_atemperado
boolean visible = false
integer x = 4645
integer y = 472
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_mant_encab_atemperado
integer x = 4197
integer y = 488
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_mant_encab_atemperado
integer x = 4197
integer y = 668
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_mant_encab_atemperado
integer x = 4197
integer y = 848
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_mant_encab_atemperado
boolean visible = false
integer x = 4197
integer y = 1176
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_mant_encab_atemperado
boolean visible = false
integer x = 4197
integer y = 1348
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_mant_encab_atemperado
integer x = 4197
integer y = 124
end type

