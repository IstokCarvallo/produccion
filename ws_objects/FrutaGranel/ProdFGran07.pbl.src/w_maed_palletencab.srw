$PBExportHeader$w_maed_palletencab.srw
forward
global type w_maed_palletencab from w_mant_encab_deta_csd
end type
type sle_mensa from statictext within w_maed_palletencab
end type
type pb_compactos from picturebutton within w_maed_palletencab
end type
type pb_cambio_folio from picturebutton within w_maed_palletencab
end type
type pb_ventana from uo_botonventanas within w_maed_palletencab
end type
end forward

shared variables 

End variables

global type w_maed_palletencab from w_mant_encab_deta_csd
string tag = "Mantención de Pallets"
integer width = 4082
integer height = 2204
string title = "Mantenedor de Pallet"
string menuname = ""
windowstate windowstate = maximized!
event ue_imprimir ( )
event ue_imprimir_tarjas ( )
event ue_cambio_folio ( )
sle_mensa sle_mensa
pb_compactos pb_compactos
pb_cambio_folio pb_cambio_folio
pb_ventana pb_ventana
end type
global w_maed_palletencab w_maed_palletencab

type variables
Integer	ii_yaexiste//, ii_cajas
Long     	il_CajasTipoPallet
Boolean	ib_Flag = True

w_mant_deta_palletfruta iw_mantencion

DataWindowChild			dw_especie, dw_etiqueta, dw_planta,dw_cliente,dw_condiciones, &
								idwc_tipoenvase, idwc_categoria, idwc_recibidor, idwc_destino, idwc_tipofrio, idwc_Camara, idwc_tippen

str_envase					istr_envase

uo_spro_serviciosplanta	iuo_serviciosplanta
uo_especie					iuo_especie
uo_tratamientofrio			iuo_tratamientofrio
uo_categorias				iuo_categorias
uo_etiquetas				iuo_etiquetas
uo_recibidores				iuo_recibidores
uo_destinos					iuo_destinos
uo_fechaMovto				iuo_FechaMovto
uo_camarasfrigo			iuo_camaras
uo_AnalizaPallet			iuo_pallet
uo_valida_codigopallet	iuo_copa
uo_ControlVentanas		iuo_Ventana
uo_EmbalajesProd			iuo_Embalajes 
end variables

forward prototypes
public function boolean existeembalaje (string as_codigo)
public subroutine habilitaingreso ()
public subroutine cuentacajas ()
public function boolean existecalibres ()
public function boolean existecondicion (integer as_valor)
public function boolean existeplanta (integer as_valor)
public function boolean existevariecalibre (integer as_valor)
public subroutine buscavariedad ()
public function boolean existepallet (string as_codigo)
public subroutine habilitaencab (boolean habilita)
public subroutine buscaenvase ()
public function boolean existevariedad (integer li_columna, integer cliente)
public function boolean noexistecliente (integer cliente)
public function boolean buscanombreembalaje (string as_embalaje)
public subroutine cargacajas ()
public function boolean existetipoembalaje (string as_codigo, boolean ab_captura)
public subroutine wf_bloqueacolumnas ()
public function string cargacodigo (integer ai_cliente, long al_planta, long al_pallet, integer ai_procedencia)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean wf_creacion_cajas ()
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "RECEPCION DE PALLETS"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_palletencab" 

vinf.dw_1.GetChild("clie_codigo", dw_cliente)
vinf.dw_1.GetChild("plde_codigo", dw_planta)
vinf.dw_1.GetChild("espe_codigo", dw_especie)
vinf.dw_1.GetChild("etiq_codigo", dw_etiqueta)
vinf.dw_1.GetChild("cond_codigo", dw_condiciones)
vinf.dw_1.GetChild("frio_tipofr", idwc_tipofrio)
vinf.dw_1.GetChild("cama_codigo", idwc_Camara)

vinf.dw_1.SetTransObject(sqlca)

dw_cliente.SetTransObject(sqlca)
dw_planta.SetTransObject(sqlca)
dw_especie.SetTransObject(sqlca)
dw_etiqueta.SetTransObject(sqlca)
dw_condiciones.SetTransObject(sqlca)
idwc_tipofrio.SetTransObject(sqlca)
idwc_Camara.SetTransObject(sqlca)

dw_cliente.Retrieve()
dw_planta.Retrieve()
dw_especie.Retrieve()
dw_etiqueta.Retrieve()
dw_condiciones.Retrieve()
idwc_tipofrio.Retrieve()
idwc_Camara.Retrieve(Long(istr_mant.argumento[6]))

Fila = vinf.dw_1.Retrieve(Long(istr_mant.argumento[2]), &
								  Long(istr_mant.argumento[6]),&
								  Long(istr_mant.argumento[1]))		

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then
	   	F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
  	End If
End If

SetPointer(Arrow!)
End event

event ue_imprimir_tarjas();Integer li_fila, li_find

If wf_Creacion_Cajas() Then
	OpenSheetWithParm(w_mant_cajasprod_imprime_elimina,"2", w_main, 1, Original!)
	w_mant_cajasprod_imprime_elimina.Visible 					= 	False
	w_mant_cajasprod_imprime_elimina.dw_lotes.DataObject	=	"dw_mues_palletfruta_elimina_imprime_alter"
	w_mant_cajasprod_imprime_elimina.dw_lotes.SetTransObject(SQLCA)
	
	w_mant_cajasprod_imprime_elimina.uo_selcliente.codigo 	= 	Long(istr_mant.argumento[1])
	w_mant_cajasprod_imprime_elimina.uo_selplanta.codigo 	=	Long(istr_mant.argumento[6])
	w_mant_cajasprod_imprime_elimina.uo_selmercado.codigo=	Integer(Istr_mant.argumento[16])
	
	w_mant_cajasprod_imprime_elimina.em_recepcion.Text		=	String(dw_2.Object.paen_numero[1])

	w_mant_cajasprod_imprime_elimina.em_recepcion.TriggerEvent("Modified")
	
	FOR li_fila = 1 TO dw_1.RowCount()
		li_find	=	w_mant_cajasprod_imprime_elimina.dw_lotes.Find("pafr_secuen = " + String(dw_1.Object.pafr_secuen[li_fila]), 1, &
																					  w_mant_cajasprod_imprime_elimina.dw_lotes.RowCount())
		If li_find > 0 Then
			w_mant_cajasprod_imprime_elimina.dw_lotes.DeleteRow(li_find)
		End If		
	NEXT
	
	w_mant_cajasprod_imprime_elimina.dw_lotes.SelectRow(0, True)
	
	w_mant_cajasprod_imprime_elimina.pb_acepta.TriggerEvent("Clicked")
	Close(w_mant_cajasprod_imprime_elimina)
	
	This.TriggerEvent("ue_recuperadatos")
End If
end event

event ue_cambio_folio();Integer 	li_fila, li_cliente
Long		ll_pallet, ll_planta, ll_antiguo

str_mant	lstr_mant

lstr_mant.Argumento[1]	=	String(dw_2.Object.clie_codigo[1])
lstr_mant.Argumento[2]	=	String(dw_2.Object.plde_codigo[1])
lstr_mant.Argumento[3]	=	String(dw_2.Object.paen_numero[1])
lstr_mant.Argumento[4]	=	String(dw_2.Object.paen_tipopa[1])

OpenWithParm(w_cambio_folio_pallets, lstr_mant)

lstr_mant = Message.PowerObjectParm

li_cliente	=	Integer(lstr_mant.Argumento[1])
ll_planta	=	Long(lstr_mant.Argumento[2])
ll_antiguo=	Long(lstr_mant.Argumento[3])
ll_pallet	=	Long(lstr_mant.Argumento[5])

If ll_pallet > 0 AND NOT IsNull(ll_pallet) Then
	DECLARE Cambio_Folio_pallet PROCEDURE FOR dbo.fgran_cambio_folio_pallet  
			@cliente	= :li_cliente,   
			@planta 	= :ll_planta,   
			@pallet 	= :ll_antiguo,   
			@nuevo 	= :ll_pallet  
		USING SQLCA;
		
		EXECUTE Cambio_Folio_pallet;
		
		If sqlca.SQLCode = -1 Then
			F_errorbasedatos(sqlca,"Actualización de Folio de Pallet")
			Rollback;
			istr_mant.argumento[2]	=	String(ll_antiguo)
		Else
			Commit;
			istr_mant.argumento[2]	=	String(ll_pallet)
			If dw_2.Object.paen_tipopa[1] = 1 Then iuo_Ventana.ActualizaCorrelativo(li_cliente, ll_planta, ll_pallet, 1)
		End If
Else
	istr_mant.argumento[2]	=	String(ll_antiguo)
End If

TriggerEvent("ue_recuperadatos")
end event

public function boolean existeembalaje (string as_codigo);//String	ls_codigo, ls_nombre
//Integer	li_cliente, li_tipoen, li_codenvase
//
//li_cliente	= Integer(istr_mant.argumento[1])
//
//SELECT	emb.emba_nombre, env.enva_tipoen, env.enva_codigo
//	INTO 	:ls_nombre, :li_tipoen, :li_codenvase
//	FROM	dbo.embalajes as emb, dbo.envases as env
//	WHERE emb.emba_codigo	= :as_codigo
//	AND	env.enva_tipoen	= emb.enva_tipoen
//	AND	env.enva_codigo	= emb.enva_codigo;
//		
//If sqlca.SQLCode = -1 Then
//	F_errorbasedatos(sqlca,"Lectura tabla Emabalajes")
//	Return False
//ElseIf sqlca.SQLCode = 100 Then
//	MessageBox("Atención","Código de Embalaje no Existe para este Cliente. Ingrese Otro.", Exclamation!, Ok!)
//	Return False
//Else
//	istr_mant.argumento[7]	=	ls_codigo
//	istr_mant.argumento[8]	=	ls_nombre
//	
//	istr_mant.argumento[27]	=	String(li_tipoen)
//	istr_mant.argumento[28]	=	String(li_codenvase)
//	
//	dw_2.SetItem(1, "emba_nombre", ls_nombre)
//	dw_2.SetItem(1, "tien_codigo", li_tipoen)
	Return True
//End If
End function

public subroutine habilitaingreso ();DateTime		ld_fecha
Boolean		lb_estado = True

dw_2.AcceptText()

//If IsNull(dw_2.Object.paen_numero[1]) OR dw_2.Object.paen_numero[1] = 0 Then
//	lb_estado = False
//End If

If IsNull(dw_2.Object.espe_codigo[1]) OR dw_2.Object.espe_codigo[1] = 0 Then
	lb_estado = False
End If

If IsNull(dw_2.Object.vari_codigo[1]) OR dw_2.Object.vari_codigo[1] = 0 Then
	lb_estado = False
End If

If IsNull(dw_2.Object.etiq_codigo[1]) OR dw_2.Object.etiq_codigo[1] = 0 Then
	lb_estado = False
End If

If IsNull(dw_2.Object.cate_codigo[1]) OR dw_2.Object.cate_codigo[1] = 0 Then
	lb_estado = False
End If

//If IsNull(dw_2.Object.cama_codigo[1]) OR dw_2.Object.cama_codigo[1] < 0 Then
//	lb_estado = False
//End If
//
//If IsNull(dw_2.Object.frio_tipofr[1]) OR dw_2.Object.frio_tipofr[1] = '' Then
//	lb_estado = False
//End If

If IsNull(dw_2.Object.dest_codigo[1]) OR dw_2.Object.dest_codigo[1] = 0 Then
	lb_estado = False
End If

If IsNull(dw_2.Object.paen_feccon[1]) OR dw_2.Object.paen_feccon[1] = Date(ld_fecha) Then
	lb_estado = False
End If

If dw_2.Object.paen_tipopa[1] = 1 AND &
	IsNull(dw_2.Object.tpen_codigo[1]) OR dw_2.Object.tpen_codigo[1] = '' Then
	lb_estado = False
End If

pb_ins_det.Enabled = lb_estado
end subroutine

public subroutine cuentacajas ();Long	ll_fila, ll_cajas

ll_cajas	=	0

FOR ll_fila = 1 TO dw_1.RowCount()
	ll_cajas = ll_cajas + dw_1.Object.pafr_ccajas[ll_fila]
NEXT

dw_2.Object.paen_ccajas[1]	=	ll_cajas
End subroutine

public function boolean existecalibres ();Integer	li_Cantidad, li_Especie, li_TipoEnvase, li_Envase

li_Especie		=	dw_2.Object.espe_codigo[1]
li_TipoEnvase	=	dw_2.Object.enva_tipoen[1]
li_Envase		=	dw_2.Object.enva_codigo[1]

SELECT	Count(caen_calibr)
	INTO	:li_Cantidad
	FROM	dbo.calibresenvase
	WHERE	espe_codigo	=	:li_Especie
	AND	enva_tipoen	=	:li_TipoEnvase
	AND	enva_codigo	=	:li_Envase ;

If SqlCa.SQLCode = -1 Then
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla CalibresEnvase")

	Return False
ElseIf li_Cantidad = 0 OR IsNull(li_cantidad) Then

	Return False
End If

Return True
End function

public function boolean existecondicion (integer as_valor); integer	li_Condicion
 String	ls_DesCondicion
 
 li_Condicion	= as_valor
 
 SELECT	cond_nombre 
 	INTO	:ls_DesCondicion  
   FROM	dbo.condicion
	WHERE	cond_codigo	=	:li_Condicion ;

If sqlca.SQLCode = 0 Then
	istr_mant.argumento[15]	=	ls_DesCondicion
Else
	istr_mant.argumento[15]	=	""
End If

Return True
End function

public function boolean existeplanta (integer as_valor); integer li_planta, li_cliente
 string	ls_desplanta
 
 li_planta	=	as_valor
 li_cliente	=	Integer(istr_mant.argumento[1])
 
 SELECT	plde_nombre
	INTO	:ls_desplanta  
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:li_planta
	AND   clie_codigo =  :li_cliente;

If sqlca.SQLCode = 0 Then
	istr_mant.argumento[14]	=	ls_desplanta
Else
	istr_mant.argumento[14]	=	""
End If	

Return TRUE
End function

public function boolean existevariecalibre (integer as_valor);Integer	li_especie, li_variedad, li_cliente
String	ls_nombre, ls_secacod, ls_calibre
Long	   registros

li_especie	=	Integer(dw_2.Object.espe_codigo[1])
li_variedad	=	as_valor
li_cliente	=	Integer(istr_mant.argumento[1])

SELECT	Count(vaca_calibr)   
	INTO	:Registros
	FROM	dbo.variecalibre
	WHERE espe_codigo	=	:li_especie
	AND	vari_codigo	=	:li_variedad ;

If (sqlca.SQLCode) = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla Variecalibre")
	Return False
	
ElseIf Registros < 1 Then
	MessageBox("Atención","Código de Variedad-Calibre no Existe.", Exclamation!, Ok!)
	Return False
End If

Return True
End function

public subroutine buscavariedad ();Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	String(dw_2.GetItemNumber(1, "espe_codigo"))
lstr_busq.argum[2]	=	String(istr_mant.argumento[1])

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	= Message.PowerObjectParm

If lstr_busq.argum[4] <> '' Then
	istr_mant.argumento[4]	=	lstr_busq.argum[3]
	istr_mant.argumento[5]	=	lstr_busq.argum[4]
	
	dw_2.setItem(1, "vari_codigo", Integer(lstr_busq.argum[3]))
	dw_2.setItem(1, "vari_nombre", lstr_busq.argum[4])
	dw_2.SetColumn("vari_codigo")
	dw_2.SetFocus()
End If
End subroutine

public function boolean existepallet (string as_codigo);Integer	li_cliente, li_cantid, li_planta, li_respuesta
Long		ll_nropal
Boolean	lb_Retorno = False

li_cliente					=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta					=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))
ll_nropal 					=	Long(as_codigo)

SELECT	Count(*)
	INTO	:li_cantid
	FROM	dbo.spro_palletencab
	WHERE	clie_codigo		= 	:li_cliente
	AND	paen_numero    = 	:ll_nropal
	AND	plde_codigo		=	:li_planta;
				
If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = True
ElseIf li_cantid > 0 Then
	
	istr_mant.argumento[2]	=	String(ll_nropal)
	
	This.TriggerEvent("ue_recuperadatos")
	
	istr_mant.argumento[3] 	= String(dw_2.Object.espe_codigo[1])
	istr_mant.argumento[4] 	= String(dw_2.Object.vari_codigo[1])
	istr_mant.argumento[5] 	= dw_2.Object.vari_nombre[1]
	istr_mant.argumento[6] 	= String(dw_2.Object.plde_codigo[1])
	istr_mant.argumento[7]  = String(dw_2.Object.enva_tipoen[1])
	istr_mant.argumento[8]  = String(dw_2.Object.enva_codigo[1])
	istr_mant.argumento[9] 	= String(dw_2.Object.etiq_codigo[1])
	istr_mant.argumento[10] = String(dw_2.Object.frio_tipofr[1])
	istr_mant.argumento[13] = String(dw_2.Object.cate_codigo[1])
	istr_mant.argumento[14] = dw_2.Object.emba_codigo[1]
	
	ExisteTipoEmbalaje(dw_2.Object.tpen_codigo[1],False)
//	ExistePlanta(dw_2.Object.plde_codigo[1])
	
	lb_retorno = FALSE
Else
	lb_retorno = FALSE
End If

Return lb_retorno
End function

public subroutine habilitaencab (boolean habilita);If Habilita Then
	dw_2.Object.clie_codigo.Protect					=	0
	dw_2.Object.paen_numero.Protect				=	0
	dw_2.Object.paen_feccon.Protect					=	0
	dw_2.Object.espe_codigo.Protect					=	0
	dw_2.Object.vari_codigo.Protect					=	0
	dw_2.Object.enva_tipoen.Protect					=	0
	dw_2.Object.enva_codigo.Protect					=	0
	dw_2.Object.tpen_codigo.Protect					=	0
	
	dw_2.Object.clie_codigo.BackGround.Color 		= 	RGB(255,255,255)
	dw_2.Object.paen_numero.BackGround.Color 	= 	RGB(255,255,255)
	dw_2.Object.paen_feccon.BackGround.Color 	= 	RGB(255,255,255)
	dw_2.Object.espe_codigo.BackGround.Color 	= 	RGB(255,255,255)
	dw_2.Object.vari_codigo.BackGround.Color 		= 	RGB(255,255,255)
	dw_2.Object.enva_tipoen.BackGround.Color 	= 	RGB(255,255,255)
	dw_2.Object.enva_codigo.BackGround.Color 	= 	RGB(255,255,255)	
	dw_2.Object.tpen_codigo.BackGround.Color 	= 	RGB(255,255,255)
	
	dw_2.Object.clie_codigo.Color 		= 	0
	dw_2.Object.paen_numero.Color 	= 	0
	dw_2.Object.paen_feccon.Color 	= 	0
	dw_2.Object.espe_codigo.Color 	= 	0
	dw_2.Object.vari_codigo.Color 		= 	0
	dw_2.Object.enva_tipoen.Color 	= 	0
	dw_2.Object.enva_codigo.Color 	= 	0
	dw_2.Object.tpen_codigo.Color 	= 	0
	
	dw_2.Object.b_buscavariedad.visible 		= 	1
	
	dw_2.SetColumn("clie_codigo")
	dw_2.SetFocus()
Else
	dw_2.Object.clie_codigo.Protect					=	1
	dw_2.Object.paen_numero.Protect				=	1
	dw_2.Object.paen_feccon.Protect					=	1
	dw_2.Object.espe_codigo.Protect					=	1
	dw_2.Object.vari_codigo.Protect					=	1
	dw_2.Object.enva_tipoen.Protect					=	1
	dw_2.Object.enva_codigo.Protect					=	1
	dw_2.Object.tpen_codigo.Protect					=	1
	
	dw_2.Object.clie_codigo.BackGround.Color 		= 	553648127
	dw_2.Object.paen_numero.BackGround.Color 	= 	553648127
	dw_2.Object.paen_feccon.BackGround.Color 	= 	553648127
	dw_2.Object.espe_codigo.BackGround.Color 	= 	553648127
	dw_2.Object.vari_codigo.BackGround.Color 		= 	553648127
	dw_2.Object.enva_tipoen.BackGround.Color 	= 	553648127
	dw_2.Object.enva_codigo.BackGround.Color 	= 	553648127
	dw_2.Object.tpen_codigo.BackGround.Color 	= 	553648127
	
	dw_2.Object.clie_codigo.Color 		= 	RGB(255,255,255)
	dw_2.Object.paen_numero.Color 	= 	RGB(255,255,255)
	dw_2.Object.paen_feccon.Color 	= 	RGB(255,255,255)
	dw_2.Object.espe_codigo.Color 	= 	RGB(255,255,255)
	dw_2.Object.vari_codigo.Color 		= 	RGB(255,255,255)
	dw_2.Object.enva_tipoen.Color 	= 	RGB(255,255,255)
	dw_2.Object.enva_codigo.Color 	= 	RGB(255,255,255)
	dw_2.Object.tpen_codigo.Color 	= 	RGB(255,255,255)
	
	dw_2.Object.b_buscavariedad.visible 		= 	0

	If dw_2.Object.paen_pmixto[1]	=	1 AND Integer(dw_2.Object.paen_pmixto.Protect)	=	1 Then
		dw_2.Object.vari_codigo.Protect				=	1
		dw_2.Object.vari_codigo.BackGround.Color 	= 	553648127
	End If
	
End If
end subroutine

public subroutine buscaenvase ();Str_busqueda	lstr_busq
Integer li_especie, li_tipo, li_envase

lstr_busq.argum[1]	=	String(dw_2.GetItemNumber(1, "enva_tipoen"))

OpenWithParm(w_busc_envases, lstr_busq)

lstr_busq	= Message.PowerObjectParm

If lstr_busq.argum[2] <> '' Then
	
  
   istr_mant.argumento[7]	=	lstr_busq.argum[1]
	istr_mant.argumento[8]	=	lstr_busq.argum[2]
	
	dw_2.SetItem(1, "enva_codigo", Integer(lstr_busq.argum[2]))
	dw_2.SetItem(1, "enva_nombre", lstr_busq.argum[3])

	dw_2.GetChild("tpen_codigo", idwc_tippen)
	idwc_tippen.SetTransObject(SqlCa)
	If idwc_tippen.Retrieve(Integer(lstr_busq.argum[1]),Integer(lstr_busq.argum[2])) = 0 Then
		idwc_tippen.InsertRow(0)
	End If	

	dw_2.SetColumn("enva_codigo")
	dw_2.SetFocus()
End If
End subroutine

public function boolean existevariedad (integer li_columna, integer cliente);Integer	li_cliente, li_especie, li_variedad
String	ls_nombre

li_especie		=	dw_2.Object.espe_codigo[1]
li_variedad		=	li_Columna

SELECT	vari_nombre
	INTO	:ls_nombre
	FROM	dbo.variedades
	WHERE espe_codigo	= 	:li_especie
	AND	vari_codigo = 	:li_variedad ;

If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla Variedades")
	Return False
ElseIf sqlca.SQLCode = 100 Then
	MessageBox("Atención","Código de Variedad no Existe. Ingrese Otra.", Exclamation!, Ok!)
	Return False
Else
	istr_mant.argumento[3]	= String(li_especie)
	istr_mant.argumento[4]	= String(li_variedad)
	istr_mant.argumento[5]	= ls_nombre
	dw_2.SetItem(1, "vari_nombre", ls_nombre)
	Return True
End If

Return False
End function

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 	clie_nombre
INTO		:ls_nombre
FROM 		dbo.clientesprod
WHERE		clie_codigo =:cliente;

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
ElseIf sqlca.SQLCode = 100 Then
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
End If

Return False
End function

public function boolean buscanombreembalaje (string as_embalaje);String	ls_nombre
Integer 	li_cliente, li_tipoen, li_envase

li_cliente = dw_2.Object.clie_codigo[1]

SELECT emba_nombre, enva_tipoen, enva_codigo  
  INTO :ls_nombre, :li_tipoen, :li_envase
  FROM dbo.embalajesprod
 WHERE emba_codigo	= :as_embalaje
 AND	 clie_codigo 	= :li_cliente;
 
If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla Embalajes")
	Return False
ElseIf sqlca.SQLCode = 100 Then
	MessageBox("Atención","Código de Embalaje no Existe para este Cliente. Ingrese Otro.", Exclamation!, Ok!)
	Return False
Else
	dw_2.Object.enva_tipoen[1]	=	li_tipoen
	dw_2.Object.enva_codigo[1]	=	li_envase
	
	istr_Mant.Argumento[7]		=	String(li_tipoen)
	istr_Mant.Argumento[8]		=	String(li_envase)
	
	dw_2.GetChild("tpen_codigo", idwc_tippen)
	
	idwc_tippen.SetTransObject(SqlCa)
	If idwc_tippen.Retrieve(li_cliente, as_embalaje) = 0 Then
		idwc_tippen.InsertRow(0) 
	End If	
	Return True
End If 
End function

public subroutine cargacajas ();//Integer	li_fila, li_cajas
//
//FOR li_fila = 1 TO dw_1.RowCount()
//	li_cajas	= li_cajas + dw_1.Object.pafr_ccajas[li_fila]
//NEXT
//
//dw_2.Object.paen_ccajas[1]	=	li_cajas
//
//istr_mant.argumento[11]	=	String(Long(istr_mant.argumento[11]) - li_cajas)
End subroutine

public function boolean existetipoembalaje (string as_codigo, boolean ab_captura);String	ls_embala
Integer	li_TipoEnvase, li_CodEnvase, li_cliente
Long		ll_altura

li_TipoEnvase	=	dw_2.Object.enva_tipoen[1]
li_CodEnvase	=	dw_2.Object.enva_codigo[1]
ls_embala		=	dw_2.Object.emba_codigo[1]
li_cliente			=	dw_2.Object.clie_codigo[1]

SELECT	tpem_cancaj, tpem_altura
	INTO	:il_CajasTipoPallet, :ll_altura
	FROM	dbo.tipopallemba
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embala
	AND	tpem_codigo	=	:as_codigo ;
		
If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla tipopallemba")
	Return False
ElseIf sqlca.SQLCode = 100 Then
	MessageBox("Atención","Tipo de Pallet no Existe.~rIngrese Otro.", Exclamation!, Ok!)
	Return False
Else
	istr_mant.Argumento[11]	= 	String(il_CajasTipoPallet)
	istr_mant.Argumento[20]	= 	String(il_CajasTipoPallet)
	dw_2.Object.paen_altura[1]	=	ll_altura
//	ii_cajas							=	il_CajasTipoPallet
	CuentaCajas()
	
	Return True
End If

Return False
end function

public subroutine wf_bloqueacolumnas ();
End subroutine

public function string cargacodigo (integer ai_cliente, long al_planta, long al_pallet, integer ai_procedencia);String ls_respuesta

DECLARE Codigo PROCEDURE FOR dbo.genera_adhesivos_pallets  
        @Planta 		= 	:al_planta,   
        @Cliente 		= 	:ai_cliente,   
        @Pallet 		= 	:al_pallet,   
        @Procedencia = 	:ai_procedencia  
	USING SQLCA;
			
EXECUTE Codigo;

If SQLCA.SQLCode = -1 Then
	F_ErrorBaseDatos(SQLCA, "Lectura del Procedimiento Almacenado genera_adhesivos_pallets" )			
Else
	FEtCH Codigo INTO :ls_respuesta;
End If	
	
CLOSE Codigo;

Return ls_respuesta 

end function

protected function boolean wf_actualiza_db (boolean borrando);String 			ls_pallet
Boolean			lb_AutoCommit, lb_Retorno
DateTime		ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_1.GrupoFecha	=	ldt_FechaHora
dw_2.GrupoFecha	=	ldt_FechaHora

If Not dw_2.uf_check_required(0) Then Return False

If Not dw_1.uf_validate(0) Then Return False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If Borrando Then
	If dw_1.Update(True, False) = 1 Then
		If dw_2.Update(True, False) = 1 Then
			Commit;
			
			If sqlca.SQLCode <> 0 Then
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			Else
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
	End If
Else
	If dw_2.Update(True, False) = 1 Then
		If dw_1.Update(True, False) = 1 Then
				
			If dw_2.Object.paen_tipopa[1] = 1 Then
				If dw_2.GetItemStatus(1, "paen_numero", Primary!) <> NotModified! Then 
					iuo_Ventana.ActualizaCorrelativo(Integer(istr_mant.Argumento[1]), Long(istr_mant.argumento[6]), dw_2.Object.paen_numero[1], 1)
				End If
			End If
			
			ls_pallet	=	String(dw_2.Object.clie_codigo[dw_2.GetRow()], '000') + String(dw_2.Object.paen_numero[dw_2.GetRow()], '000000')
								
			If iuo_pallet.analiza_datos(ls_pallet, SqlCa) Then dw_2.Object.paen_sscc18[1] =	iuo_pallet.Codbarra
			
			dw_2.Object.paen_gs1128[dw_2.GetRow()]	=	CargaCodigo(dw_2.Object.clie_codigo[dw_2.GetRow()], dw_2.Object.plde_codigo[dw_2.GetRow()], &
																					dw_2.Object.paen_numero[dw_2.GetRow()], 1)
			Commit;
			
			dw_2.ResetUpdate()
				
			If sqlca.SQLCode <> 0 Then
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			Else
				If dw_2.Update(True, False) = 1 Then
					Commit;
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
				Else
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				End If
			End If
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	End If
End If

sqlca.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
end function

public function boolean wf_creacion_cajas ();Integer 	li_fila, li_cliente, li_secuencia, li_control
Long		ll_pallet, ll_planta
String 	ls_pcname, ls_Formato

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", "ComputerName", RegString!, ls_pcname)

li_cliente	=	dw_2.Object.clie_codigo[1]
ll_planta	=	dw_2.Object.plde_codigo[1]
ll_pallet	=	dw_2.Object.paen_numero[1]

SELECT loco_dwcomp
  INTO :ls_Formato
  FROM dbo.spro_correlcompequipo
 WHERE plde_codigo = :ll_planta
   AND Upper(equi_nombre) = Upper(:ls_pcname);
	
If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla Correativos de Compactos")
	Return False
	
ElseIf sqlca.SQLCode = 100 Then
	MessageBox("Atención", "Nombre de Equipo No Tiene Asignado Formato de Compactos, Debe Asignar previamente.", &
					Exclamation!, OK!)
	Return False
	
Else
	FOR li_fila	=	1 TO dw_1.RowCount()
		li_secuencia	=	dw_1.Object.pafr_secuen[li_fila]
		
		If li_secuencia > 999 Then
			MEssageBox("Error de datos", "Imposible crear caja, ya que el correlativo indica que es una caja valida")
		Else	
			DECLARE creacioncaja PROCEDURE FOR dbo.fgran_creacion_caja_porbultos  
				@Cliente 	= :li_cliente,   
				@planta 		= :ll_planta,   
				@Pallet 		= :ll_pallet,   
				@Secuencia 	= :li_secuencia,   
				@computador = :ls_pcname 
			USING SQLCA;
			
			EXECUTE creacioncaja;
			
			If SQLCA.SQLCode = -1 Then
				F_ErrorBaseDatos(sqlca, "Lectura del Procedimiento Almacenado fgran_creacion_caja_porbultos" )
										
				sle_mensa.text	=	"Secuencia " + String(li_secuencia) + " imposible de crear"
			Else
				sle_mensa.text	=	"Secuencia " + String(li_secuencia) + " creada satisfactoriamente"
				li_control	++
			End If	
			
			CLOSE creacioncaja;
		End If
	NEXT
	
	If li_control = dw_1.RowCount() Then
		COMMIT;
		Return True
	Else
		ROLLBACK;
		Return False
	End If
End If
end function

event open;/*
		Argumentos					:		[1]	=	Código de Exportador
												[2]	=	Numero de Pallet
												[3]	=	Código de Especie
												[4]	=	Código de Variedad
												[5]	=	Descripcion de Variedad
												[6]	=	Código de Planta
												[7]	=	Código de Tipo de Envase
												[8]	=	Código de Envase
												[9]	=	Código de Etiqueta
												[10]	=	Código de Tipo Frio
												[11]	=	Cantidad de Cajas
												[12]	=	Pallet Mixto 0 = No - 1 = Si
												[13]	=	Código de Categoria
												[15]	=	Código de Embalaje
												[16]	=	Código de Mercado
*/

x												= 	0
y												= 	0
This.Height									= 	2550
im_menu										= 	m_principal
This.Icon										=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True


iuo_serviciosplanta=	Create uo_spro_serviciosplanta
iuo_especie			=	Create uo_especie
iuo_tratamientofrio=	Create uo_tratamientofrio
iuo_categorias		=	Create uo_categorias
iuo_etiquetas		=	Create uo_etiquetas
iuo_recibidores		=	Create uo_recibidores
iuo_destinos			=	Create uo_destinos
iuo_FechaMovto	=	Create uo_FechaMovto
iuo_camaras 		=	Create uo_camarasfrigo
iuo_pallet			=	Create uo_AnalizaPallet
iuo_copa				=	Create uo_valida_codigopallet
iuo_Ventana			=	Create uo_ControlVentanas
iuo_Embalajes		=	Create uo_EmbalajesProd

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.ModIfy("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(FocusRect!)

istr_mant.dw								=	dw_1
istr_mant.solo_consulta					=	False
istr_mant.argumento[25]					=	''

pb_nuevo.PostEvent(Clicked!)

dw_2.GetChild("clie_codigo", dw_cliente)
dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("espe_codigo", dw_especie)
dw_2.GetChild("etiq_codigo", dw_etiqueta)
dw_2.GetChild("tpen_codigo", idwc_tippen)
dw_2.GetChild("enva_tipoen", idwc_tipoenvase)
dw_2.GetChild("cate_codigo", idwc_categoria)
dw_2.GetChild("reci_codigo", idwc_recibidor)
dw_2.GetChild("dest_codigo", idwc_destino)
dw_2.GetChild("frio_tipofr", idwc_tipofrio)
dw_2.GetChild("cama_codigo", idwc_Camara)

dw_cliente.SetTransObject(sqlca)
dw_planta.SetTransObject(sqlca)
dw_especie.SetTransObject(sqlca)
dw_etiqueta.SetTransObject(sqlca)
idwc_tipoenvase.SetTransObject(sqlca)
idwc_tippen.SetTransObject(sqlca)
idwc_categoria.SetTransObject(sqlca)
idwc_recibidor.SetTransObject(sqlca)
idwc_destino.SetTransObject(sqlca)
idwc_tipofrio.SetTransObject(sqlca)
idwc_Camara.SetTransObject(sqlca)

dw_cliente.Retrieve()
dw_planta.Retrieve()
dw_especie.Retrieve()
dw_etiqueta.Retrieve()
idwc_tippen.InsertRow(0)
idwc_tipoenvase.Retrieve()
idwc_categoria.Retrieve()
idwc_recibidor.Retrieve()
idwc_destino.Retrieve()
idwc_tipofrio.Retrieve()
idwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_2.object.plde_codigo[1]				= 	gstr_ParamPlanta.CodigoPlanta
istr_mant.argumento[1]					=	String(gi_CodExport)
istr_mant.argumento[6]					=	String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[10] 				= 	'1'
istr_mant.argumento[12] 				= 	'0'
istr_mant.argumento[13] 				= 	'1'
istr_mant.argumento[26] 				= 	''
istr_mant.argumento[27] 				= 	''
istr_mant.argumento[28] 				= 	''

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"
End event

event ue_borra_detalle;If dw_1.rowcount() < 1 Then
	dw_1.SetFocus()
	Return
End If

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

If Message.DoubleParm = -1 Then Return

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

If istr_mant.respuesta = 1 Then
	If dw_1.DeleteRow(0) = 1 Then
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	Else
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	End If

	If dw_1.RowCount() = 0 Then 
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	End If
End If

istr_mant.borra	 = False
End event

event ue_nuevo_detalle;istr_mant.borra		= False
istr_mant.agrega	= True
CuentaCajas()

If ExisteCalibres() Then
	istr_mant.Argumento[90]		= 	String(dw_2.Object.paen_ccajas[1])
	istr_mant.Argumento[91]		= 	String(dw_2.Object.paen_feccon[1], 'dd/mm/yyyy')	
	If IsNull(istr_mant.Argumento[90]) OR Integer(istr_mant.Argumento[90]) < 0 Then istr_mant.Argumento[90]		= 	'0'
	
	OpenWithParm(iw_mantencion, istr_mant)
	
	If dw_1.RowCount() > 0 Then HabilitaEncab(False)
	
	If dw_1.RowCount() > 0 and Not pb_eliminar.Enabled Then
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled	= True
	End If
	
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
	CuentaCajas()
Else
	MessageBox("Atención", "Falta Registrar Calibres para Especie (" + & 
					String(dw_2.Object.espe_codigo[1], '00') + ") - Tipo de Envase (" + &
					String(dw_2.Object.enva_tipoen[1]) + ") - Envase (" + String(dw_2.Object.enva_codigo[1], '000') + ")")
End If
End event

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e
Integer	li_Respuesta

DO
	dw_2.SetRedraw(False)
	
	dw_2.Reset()
	
	ll_fila_e	=	dw_2.Retrieve(Long(istr_mant.argumento[2]), Long(istr_mant.argumento[6]),Long(istr_mant.argumento[1]))
	
	If ll_fila_e = -1 Then
		li_Respuesta	=	MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	ElseIf ll_fila_e < 1 Then
		Messagebox("Advertencia", "El pallet ingresado no existe.~r~nFavor ingresar otra tarja", StopSign!)
	Else
		If  iuo_destinos.Existe(dw_2.Object.dest_codigo[1],True,SqlCa) Then
			 Istr_mant.argumento[16]	=	String(iuo_destinos.CodigoMercado)
		End If
		If ll_fila_e > 0 Then
			
			If isnull(dw_2.Object.paen_inspec[1]) Then
				dw_2.Object.paen_inspec[1] = 0
			ElseIf dw_2.Object.paen_inspec[1] > 0 Then
				dw_2.Object.dest_codigo.Protect 				= 	1
				dw_2.Object.dest_codigo.Background.Color 	= 	RGB(192,192,192)
			Else
				dw_2.Object.dest_codigo.Protect 				= 	0
				dw_2.Object.dest_codigo.Background.Color 	=	RGB(255,255,255)
			End If	
				
			istr_mant.Argumento[12]	=	String(dw_2.Object.paen_pmixto[1])
			istr_mant.Argumento[13]	=	String(dw_2.Object.cate_codigo[1])
			istr_mant.Argumento[14]	=	dw_2.Object.emba_codigo[1]
			
			If dw_2.Object.paen_estado[1]	=	2 Then
				istr_Mant.Solo_Consulta	=	True
			Else
				istr_Mant.Solo_Consulta	=	False
			End If
			
			ExisteEnvase(dw_2.Object.enva_tipoen[1],dw_2.Object.enva_codigo[1],istr_envase)
			
			dw_2.GetChild("tpen_codigo",idwc_tippen)
			idwc_tippen.SetTransObject(SQLCA)
			If idwc_tippen.Retrieve(dw_2.Object.clie_codigo[1], dw_2.Object.emba_codigo[1]) = 0 Then
				idwc_tippen.InsertRow(0)
			End If	
			
			ExisteTipoEmbalaje(dw_2.Object.tpen_codigo[1],True)
		End If
		
		DO
			//////////////////////////////////////////////////////////////////
			ll_fila_d	=	dw_1.Retrieve(	Long(istr_mant.argumento[1]), &
													Long(istr_mant.argumento[2]),&
													Long(istr_mant.argumento[6]))

			If ll_fila_d = -1 Then
				li_Respuesta	=	MessageBox("Error en Base de Datos", &
												"No es posible conectar la Base de Datos 2.", &
												Information!, RetryCancel!)
			Else
				pb_eliminar.Enabled			=	NOT istr_Mant.Solo_Consulta
				pb_grabar.Enabled			=	NOT istr_Mant.Solo_Consulta
				pb_ins_det.Enabled			=	NOT istr_Mant.Solo_Consulta
				pb_ventana.Enabled			=	True
				pb_imprimir.Enabled			=	True
				pb_compactos.Enabled		=	True
				pb_cambio_folio.Enabled		=	True

				If ll_fila_d > 0 Then
					pb_eli_det.Enabled	=	NOT istr_Mant.Solo_Consulta
					dw_1.SetRow(1)
					dw_1.SelectRow(1, True)
					dw_1.SetFocus()
					
					HabilitaEncab(False)
					CuentaCajas()
					If dw_2.GetItemStatus(1, 0, Primary!) = DataModIfied! Then
						If dw_2.Update(True, False) = 1 Then
							Commit;
							
							If sqlca.SQLCode <> 0 Then
								F_ErrorBaseDatos(sqlca, This.Title)
								
								RollBack;
							Else
								dw_2.ResetUpdate()
							End If
						Else
							F_ErrorBaseDatos(sqlca, This.Title)
							
							RollBack;
						End If
					End If
					
					
					/*
					control de boton de emision de tarjas	
					*/
					pb_ventana.Enabled 			=	dw_2.Object.paen_tipopa[1]	=	1
					
					pb_ventana.ii_cliente		=	dw_2.Object.clie_codigo[1]
					pb_ventana.ii_especie		=	dw_2.Object.espe_codigo[1]
					pb_ventana.ii_cajas			=	dw_2.Object.paen_ccajas[1]
					pb_ventana.il_planta			=	dw_2.Object.plde_codigo[1]
					pb_ventana.il_pallet			=	dw_2.Object.paen_numero[1]
					pb_ventana.ii_procedencia	=	1//Granel
					pb_ventana.ii_operacion		=	1//Impresion
					pb_ventana.ii_sistema		=	1//Granel	
					/*
					fin control
					*/
					
				Else
					pb_grabar.Enabled	= FALSE
					//pb_ins_det.SetFocus()
				End If
			End If
		LOOP WHILE li_Respuesta = 1

		If li_Respuesta = 2 Then Close(This)
	End If
	
	dw_2.SetRedraw(True)
LOOP WHILE li_Respuesta = 1

If li_Respuesta = 2 Then Close(This)
end event

on w_maed_palletencab.create
int iCurrent
call super::create
this.sle_mensa=create sle_mensa
this.pb_compactos=create pb_compactos
this.pb_cambio_folio=create pb_cambio_folio
this.pb_ventana=create pb_ventana
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.sle_mensa
this.Control[iCurrent+2]=this.pb_compactos
this.Control[iCurrent+3]=this.pb_cambio_folio
this.Control[iCurrent+4]=this.pb_ventana
end on

on w_maed_palletencab.destroy
call super::destroy
destroy(this.sle_mensa)
destroy(this.pb_compactos)
destroy(this.pb_cambio_folio)
destroy(this.pb_ventana)
end on

event ue_nuevo;call super::ue_nuevo;HabilitaEncab(True)

dw_2.SetItem(1, "clie_codigo", Integer(istr_mant.Argumento[1]))
dw_2.SetItem(1, "plde_codigo", Integer(istr_mant.Argumento[6]))
dw_2.SetItem(1, "espe_codigo", gstr_paramplanta.CodigoEspecie)

istr_mant.argumento[3]	=	String(gstr_paramplanta.CodigoEspecie)

il_CajasTipoPallet								=	0

dw_2.Object.vari_codigo.Protect				=	0
dw_2.Object.vari_codigo.BackGround.Color 	= 	RGB(255,255,255)

dw_2.Object.paen_numero.Protect = 1
dw_2.Object.paen_numero.Background.Color	= 553648127

//dw_2.Object.paen_feccon[1]						=	Date(String(Today(), 'dd/mm/yyyy'))

idwc_Camara.SetTransObject(Sqlca)
idwc_Camara.Retrieve(Integer(istr_mant.Argumento[6]))

pb_cambio_folio.Enabled		=	False
ib_Flag = True
End event

event ue_seleccion;call super::ue_seleccion;str_busqueda lstr_busq

lstr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1]) 
lstr_busq.argum[2]	=	""
lstr_busq.argum[3]	=	""
lstr_busq.argum[4]	=	""
lstr_busq.argum[5]	=	String(dw_2.Object.plde_codigo[1])

OpenWithParm(w_busc_palletencab, lstr_busq)

lstr_busq = Message.PowerObjectParm

If lstr_busq.argum[2] <> "" Then
	istr_mant.argumento[1] = lstr_busq.argum[1]
	istr_mant.argumento[2] = lstr_busq.argum[2]
	//istr_mant.argumento[6] = lstr_busq.argum[7]
	
	This.TriggerEvent("ue_recuperadatos")
	ExistePallet(String(dw_2.Object.paen_numero[1]))
Else
	pb_buscar.SetFocus()
End If
End event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_Fila, ll_Numero, ll_Correlativo
Integer	li_Planta, li_cliente, li_Secuencia
String		ls_Null

Message.DoubleParm	=	0

SetNull(ls_Null)

If dw_1.RowCount() <= 0 Then
	Message.DoubleParm	=	-1
	Return
End If

CuentaCajas()

If dw_1.Object.Total_cajas[1] = il_CajasTipoPallet Then //	Pallet CompleTo
	dw_2.Object.paen_tipopa[1]	=	1
	
	If IsNull(dw_2.Object.paen_numero[1]) Then
		If dw_2.Object.paen_tipopa[1] = 1 Then
			ll_Correlativo =	iuo_Ventana.Correlativo(Integer(istr_mant.Argumento[1]), Long(istr_mant.argumento[6]), dw_2.Object.paen_tipopa[1])
			If ll_Correlativo <> -1 Then
				dw_2.Object.paen_numero[1]	= ll_Correlativo
				istr_Mant.Argumento[2]			=	String(dw_2.Object.paen_numero[1])
				iuo_Ventana.of_Disponible(Integer(istr_mant.Argumento[1]), Long(istr_mant.argumento[6]), dw_2.Object.paen_tipopa[1], SQLCA)
			Else
				Message.DoubleParm	=	-1
				Return
			End If
		End If
	End If
ElseIf dw_1.Object.Total_cajas[1] > il_CajasTipoPallet Then
	MessageBox("Error", "Cantidad de Cajas del Detalle no corresponde~ra Cantidad de Cajas del Tipo de Pallet.~r~r" + &
					"Revise Detalle o Seleccione otro Tipo de Pallet.")
	Message.DoubleParm	=	-1
	Return
Else 	//	Pallet Pucho
	dw_2.Object.paen_tipopa[1]			=	2
	
	If ib_Flag Then
		dw_2.Object.paen_numero.Protect	=	0
		dw_2.Object.paen_numero.Color		=	0
		dw_2.Object.paen_numero.Background.Color	= RGB(255,255,255)
		dw_2.SetItem(1, "paen_numero", Integer(ls_Null))
		
		MessageBox("Error", "Debe Ingresar Numero de Pallet Pucho.", StopSign!, OK!)
		
		ib_Flag = False
		Message.DoubleParm	=	-1
		Return
	End If
End If

li_cliente		=	dw_2.Object.clie_codigo[1] 
ll_Numero	=	dw_2.Object.paen_numero[1]
li_Planta		=	dw_2.Object.plde_codigo[1]

If IsNull(dw_2.Object.cama_codigo[1]) Then dw_2.Object.cama_codigo[1]	=	0

SELECT	IsNull(Max(pafr_secuen), 0) + 1
	INTO	:li_Secuencia
	FROM	dbo.spro_palletfruta
	WHERE	clie_codigo	=	:li_cliente
	AND	paen_numero	=	:ll_Numero
	AND	plde_codigo	=	:li_Planta ;
	
dw_2.Object.esta_codigo[1]		=	1 // Estacion Packing
dw_2.Object.paen_fecpro[1]	=	Datetime(Today(), Now())

For ll_Fila = 1 To dw_1.RowCount()
	If dw_1.GetItemStatus(ll_Fila,0,Primary!) = NewModIfied! Then
		
		dw_1.Object.clie_codigo[ll_Fila]		=	dw_2.Object.clie_codigo[1]
		dw_1.Object.plde_codigo[ll_Fila]		=	dw_2.Object.plde_codigo[1]
		dw_1.Object.paen_numero[ll_Fila]	=	dw_2.Object.paen_numero[1]
		dw_1.Object.espe_codigo[ll_Fila]		=	dw_2.Object.espe_codigo[1]
		dw_1.Object.etiq_codigo[ll_Fila]		=	dw_2.Object.etiq_codigo[1]
		dw_1.Object.paen_numero[ll_Fila]	=	dw_2.Object.paen_numero[1]
		dw_1.Object.pafr_copack[ll_Fila]		=	li_Planta
		dw_1.Object.pafr_secuen[ll_Fila]		=	ll_Fila//li_Secuencia
	
		li_Secuencia ++
	End If
Next

end event

event activate;If dw_1.rowcount() > 0 Then
	pb_eli_det.enabled = true
End If
End event

event ue_modIfica_detalle;
If dw_1.RowCount() > 0 Then
	istr_mant.agrega = False
	
	istr_mant.Argumento[20]		= 	String(dw_2.Object.paen_ccajas[1])
	istr_mant.Argumento[91]		= 	String(dw_2.Object.paen_feccon[1], 'dd/mm/yyyy')
	
	If IsNull(istr_mant.Argumento[20]) OR Integer(istr_mant.Argumento[20]) < 0 Then
		istr_mant.Argumento[20]		= 	'0'
	End If
	
	OpenWithParm(iw_mantencion, istr_mant)
	cuentacajas()
End If
End event

event ue_guardar;If dw_1.AcceptText() = -1 Then Return

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

If Message.DoubleParm = -1 Then Return

If wf_actualiza_db(False) Then
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled		=	True
	pb_imprimir.Enabled		=	True
	pb_compactos.Enabled	=	True
Else
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	Return
End If
End event

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0, &
			li_Ancho = 245, li_Alto = 200, li_Siguiente = 210

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	dw_1.width	=	This.WorkSpaceWidth() - 600
	maximo		=	dw_1.width
END IF

dw_2.x					=	37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					=	37

dw_1.x					=	37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					=	64 + dw_2.Height
dw_1.height				=	This.WorkSpaceHeight() - dw_1.y - 41


li_posic_x				=	This.WorkSpaceWidth() - 370
li_posic_y				=	30 

IF pb_buscar.Visible THEN
	pb_buscar.x				=	li_posic_x
	pb_buscar.y				=	li_posic_y
	pb_buscar.width		=	li_Ancho
	pb_buscar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				=	li_posic_x
	pb_nuevo.y				=	li_posic_y
	pb_nuevo.width			=	li_Ancho
	pb_nuevo.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			=	li_posic_x
	pb_eliminar.y			=	li_posic_y
	pb_eliminar.width		=	li_Ancho
	pb_eliminar.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				=	li_posic_x
	pb_grabar.y				=	li_posic_y
	pb_grabar.width		=	li_Ancho
	pb_grabar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			=	li_posic_x
	pb_imprimir.y			=	li_posic_y
	pb_imprimir.width		=	li_Ancho
	pb_imprimir.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_Compactos.Visible THEN
	pb_Compactos.x				=	li_posic_x
	pb_Compactos.y				=	li_posic_y
	pb_Compactos.width			=	li_Ancho
	pb_Compactos.height			=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_Ventana.Visible THEN
	pb_Ventana.x				=	li_posic_x
	pb_Ventana.y				=	li_posic_y
	pb_Ventana.width			=	li_Ancho
	pb_Ventana.height			=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_cambio_folio.Visible THEN
	pb_cambio_folio.x				=	li_posic_x
	pb_cambio_folio.y				=	li_posic_y
	pb_cambio_folio.width			=	li_Ancho
	pb_cambio_folio.height			=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_salir.Visible THEN
	pb_salir.x				=	li_posic_x
	pb_salir.y				=	li_posic_y
	pb_salir.width			=	li_Ancho
	pb_salir.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

pb_eli_det.x				=	li_posic_x
pb_eli_det.y				=	dw_1.y + dw_1.Height - li_Siguiente
pb_eli_det.width		=	li_Ancho
pb_eli_det.height		=	li_Alto

pb_ins_det.x			=	li_posic_x
pb_ins_det.y			=	pb_eli_det.y - li_Siguiente - 10
pb_ins_det.width		=	li_Ancho
pb_ins_det.height		=	li_Alto


end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_palletencab
integer x = 101
integer y = 904
integer width = 3291
integer height = 1196
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_palletfruta"
end type

event dw_1::doubleclicked;If NOT Isnull(This.Object.Prod_codigo[row]) Then
	istr_mant.argumento[25] = String(This.Object.Prod_codigo[row])
Else
	istr_mant.argumento[25] = '0'
End If	
If NOT Isnull(This.Object.prod_codrot[row]) Then
	istr_mant.argumento[26] = String(This.Object.prod_codrot[row])
Else
	istr_mant.argumento[26] = '0'	
End If
If NOT Isnull(This.Object.prbr_codpre[row]) Then
	istr_mant.argumento[27] = String(This.Object.prbr_codpre[row])
Else
	istr_mant.argumento[27] = '0'	
End If
If NOT Isnull(This.Object.pafr_huert1[row]) Then
	istr_mant.argumento[28] = String(This.Object.pafr_huert1[row])
Else
	istr_mant.argumento[28] = '0'	
End If	

Parent.TriggerEvent("ue_modIfica_detalle")

Return 0
End event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_palletencab
integer x = 261
integer y = 40
integer width = 2917
integer height = 768
string dataobject = "dw_mant_palletencab"
boolean hsplitscroll = true
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null, ll_fila
String	ls_columna, ls_Null, ls_Fecha
Date 		ldt_fecha

DataWIndowChild	dw_calibres

SetNull(ll_Null)
SetNull(ls_Null)

ls_columna = dwo.name
ii_yaexiste	=	0

Choose Case ls_columna
	Case "clie_codigo"
		If NoExisteCliente(Integer(data)) Then
			This.SetItem(row,"clie_codigo",Integer(ls_Null))
			Return 1
		Else
			dw_2.GetChild("espe_codigo", dw_especie)
			dw_especie.SetTransObject(sqlca)
			dw_especie.Retrieve(integer(data))
			istr_mant.Argumento[1] = data
		End If	
		
	Case "paen_tipopa"
		If Data = '1' Then
			This.Object.paen_numero.Protect = 1
			This.Object.paen_numero.Background.Color	= 553648127
			This.SetItem(Row, "paen_numero", Integer(ls_Null))
		Else
			This.Object.paen_numero.Protect = 0
			This.Object.paen_numero.Background.Color	= RGB(255,255,255)
			This.SetItem(Row, "paen_numero", Integer(ls_Null))
		End If
		
	Case "paen_numero"
		If dw_2.Object.paen_tipopa[1] = 1 Then
			If ExistePallet(data)  Or Not iuo_Ventana.ValidaRango(Integer(istr_mant.Argumento[1]), Long(istr_mant.argumento[6]), Long(Data), 1, 0, 0, 0) Then
				ii_yaexiste = 1
				dw_2.SetItem(1, "paen_numero", ll_null)
				Return 1
			Else
				iuo_Ventana.of_Disponible(Integer(istr_mant.Argumento[1]), Long(istr_mant.argumento[6]), dw_2.Object.paen_tipopa[1], SQLCA)
			End If
		End If
		
		istr_mant.Argumento[2] = data
		
	Case "paen_feccon"
		ldt_Fecha	=	Date(Mid(data,1,10))
		
		If NOT iuo_FechaMovto.Valida_FechaMovto(ldt_Fecha) Then
			This.SetItem(1,"paen_feccon",Date(ls_Null))
			This.SetFocus()
			Return 1
		End If
		
		ls_Fecha	=	Data		
		This.SetItem(1, ls_Columna, Date(Mid(ls_Fecha,1,10)))
	
	Case "espe_codigo"
		If iuo_especie.Existe(Integer(Data),True,SqlCa) Then
			istr_mant.argumento[3]	= data
		Else
			dw_2.SetItem(1, ls_Columna, Integer(ls_Null))
			dw_2.SetItem(1, "vari_nombre", "")
			dw_2.SetItem(1, "vari_codigo", ll_null)
			Return 1
		End If

	Case "vari_codigo"
		If NOT ExisteVariedad(Integer(data),gi_codexport) Then
			dw_2.SetItem(1, "vari_nombre", "")
			dw_2.SetItem(1, "vari_codigo", ll_null)
			Return 1
		End If
					
	Case "plde_codigo"
		istr_mant.argumento[6]	=	data
		idwc_Camara.Retrieve(data)
		
	Case "emba_codigo"
		If Not iuo_Embalajes.Existe(Integer(istr_Mant.Argumento[1]), Data, True, Sqlca) Then 
			This.SetItem(Row, ls_Columna, ls_Null)
			Return 1
		Else
			This.Object.enva_tipoen.Color 	= 	RGB(255,255,255)
			This.Object.enva_codigo.Color 	= 	RGB(255,255,255)
			
			This.SetItem(Row, 'copa_codigo', iuo_Embalajes.BasePallet)
			This.Object.enva_tipoen[1]	=	iuo_Embalajes.TipoEnvase
			This.Object.enva_codigo[1]	=	iuo_Embalajes.CodEnvase
		
			istr_Mant.Argumento[7]		=	String(iuo_Embalajes.TipoEnvase)
			istr_Mant.Argumento[8]		=	String(iuo_Embalajes.CodEnvase)
	
			This.GetChild("tpen_codigo", idwc_tippen)
			idwc_tippen.SetTransObject(SqlCa)
			If idwc_tippen.Retrieve(Integer(iuo_Embalajes.CodCliente), iuo_Embalajes.Codigo) = 0 Then idwc_tippen.InsertRow(0) 
	
			istr_mant.argumento[14]	=	iuo_Embalajes.Codigo
		End If

	Case "enva_tipoen"
		If NOT ExisteEnvase(Integer(data),0,istr_envase) Then
			This.SetItem(1, ls_columna, ll_null)
			Return 1
		Else
			istr_Mant.Argumento[7]	=	Data
		End If

	Case "enva_codigo"
		This.Object.tpen_codigo[1]	=	ls_Null
		If NOT ExisteEnvase(This.Object.enva_tipoen[1],Integer(Data),istr_envase) Then
			This.SetItem(1, "enva_nombre", "")
			This.SetItem(1, ls_Columna, ll_null)
			Return 1
		Else
			istr_Mant.Argumento[8]	=	Data
			This.SetItem(1, "enva_nombre", istr_envase.Nombre)
			dw_2.GetChild("tpen_codigo", idwc_tippen)
			idwc_tippen.SetTransObject(SqlCa)
		 	If idwc_tippen.Retrieve(istr_envase.TipoEnvase,istr_envase.Codigo)=0 Then
				idwc_tippen.InsertRow(0) 
			End If	
		End If

	Case "tpen_codigo"
//		If dw_2.object.paen_tipopa[Row] = 1 Then
			If ExisteTipoEmbalaje(data, False) = False Then
				dw_2.SetItem(1, ls_Columna, ls_Null)
				Return 1
//			End If
		End If

	Case "etiq_codigo"
		If iuo_etiquetas.Existe(Integer(Data),True,SqlCa) Then
			istr_mant.argumento[9]	= data
			FOR	ll_fila	=	1	TO	dw_1.RowCount()
				dw_1.Object.etiq_codigo[ll_fila]	=	Integer(data)
			NEXT
		Else
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		End If
		
	Case "frio_tipofr"
		If iuo_tratamientofrio.ofp_recupera_tratamientofrio(SqlCA,Data,True) Then
			istr_mant.argumento[10]	= data
		Else
			This.SetItem(1, ls_Columna, ls_Null)
			Return 1
		End If

	Case "cate_codigo"
		If iuo_categorias.Existe(Integer(Data),True,SqlCa) Then
			istr_mant.argumento[13]	= data
		Else
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		End If
		
	Case "paen_ccajas"
		istr_mant.argumento[11]	= data
			
	Case "paen_pexpor"
		istr_mant.argumento[22]	= data
			
	Case "paen_pmixto"
		istr_mant.argumento[12]	= data

	Case "sepl_codigo"
		If NOT iuo_serviciosplanta.Existe(Integer(Data),True,SqlCa) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		End If

	Case "reci_codigo"
		If NOT iuo_recibidores.Existe(Long(Data),True,SqlCa) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		End If

	Case "dest_codigo"
		If NOT iuo_destinos.Existe(Integer(Data),True,SqlCa) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		Else
			 Istr_mant.argumento[16]	=	String(iuo_destinos.CodigoMercado)
		End If
	
	Case "cama_codigo"
		If NOT iuo_camaras.Existe(This.Object.plde_codigo[1], Integer(data), True, SQLCA) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		Else
			This.Object.frio_tipofr[1]	=	iuo_camaras.TipoFrio
			istr_mant.argumento[10]=	String(iuo_camaras.TipoFrio)
		End If
		
	Case "copa_codigo"
		If NOT iuo_copa.Existe(Integer(data), True, SQLCa) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		End If
			
End Choose
	
HabilitaIngreso()
end event

event dw_2::doubleclicked;//
End event

event dw_2::buttonclicked;call super::buttonclicked;str_mant lstr_mant

Choose Case dwo.name
		
	Case "b_buscavariedad"
		If dw_2.Object.paen_pmixto[1]	=	1 AND Integer(dw_2.Object.paen_pmixto.Protect)	=	1 Then Return
		buscavariedad()

	Case "b_buscaenvase"
		buscaenvase()
		
End Choose
End event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_palletencab
integer x = 3712
integer y = 216
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_palletencab
integer x = 3712
integer y = 404
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_palletencab
integer x = 3712
integer y = 592
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_palletencab
integer x = 3712
integer y = 780
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_palletencab
integer x = 3712
integer y = 1532
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_palletencab
integer x = 3712
integer y = 1720
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_palletencab
integer x = 3712
integer y = 1908
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_palletencab
integer x = 3712
integer y = 28
end type

type sle_mensa from statictext within w_maed_palletencab
boolean visible = false
integer x = 101
integer y = 1140
integer width = 2985
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type pb_compactos from picturebutton within w_maed_palletencab
string tag = "Impresión de Adhesivos"
integer x = 3712
integer y = 968
integer width = 302
integer height = 244
integer taborder = 100
boolean bringtotop = true
integer textsize = -6
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "ComputeToday!"
string disabledname = "ComputeToday!"
vtextalign vtextalign = vcenter!
end type

event clicked;Parent.TriggerEvent("ue_imprimir_tarjas")
end event

type pb_cambio_folio from picturebutton within w_maed_palletencab
string tag = "Cambio de Folio Pallet"
integer x = 3712
integer y = 1344
integer width = 302
integer height = 244
integer taborder = 110
boolean bringtotop = true
integer textsize = -6
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "FullBuild!"
string disabledname = "FullBuild!"
vtextalign vtextalign = vcenter!
end type

event clicked;Parent.TriggerEvent("ue_cambio_folio")
End event

type pb_ventana from uo_botonventanas within w_maed_palletencab
integer x = 3712
integer y = 1156
integer width = 302
integer height = 244
integer taborder = 120
boolean bringtotop = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Adhesivo.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Adhesivo-bn.png"
end type

