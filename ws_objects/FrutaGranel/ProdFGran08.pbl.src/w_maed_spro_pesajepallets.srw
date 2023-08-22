$PBExportHeader$w_maed_spro_pesajepallets.srw
forward
global type w_maed_spro_pesajepallets from w_mant_encab_deta_csd
end type
end forward

global type w_maed_spro_pesajepallets from w_mant_encab_deta_csd
integer width = 2766
integer height = 1900
string title = "Pesaje de Pallets"
string menuname = ""
end type
global w_maed_spro_pesajepallets w_maed_spro_pesajepallets

type variables
uo_lineapacking 						iuo_LineaPacking	
uo_spro_ordenproceso				iuo_ordenproceso	
uo_plantadesp							iuo_plantalote

datawindowchild       					idwc_planta, idwc_linea, idwc_tipoenva, idwc_envase, idwc_calidad

Datetime 					 			idt_fechasistema

w_mant_deta_spro_pesajepallets	iw_detalles
str_mant									istr_mant2
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public function long buscabultos (string as_lote, integer ai_tipoenva, integer ai_enva)
public function boolean duplicado (string as_columna, string as_valor)
public function boolean habilitabultos (string as_columna, string as_valor)
public subroutine buscaorden ()
public subroutine habilitadetalle (string as_columna)
public function boolean noexistecliente (integer al_codigo)
protected function boolean wf_actualiza_db (boolean borrando)
end prototypes

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect				=	0
	dw_2.Object.orpr_tipord.Protect				=	0
	dw_2.Object.orpr_numero.Protect				=	0
	
	dw_2.Object.clie_codigo.Color		=	0
	//dw_2.Object.orpr_tipord.Color		=	0
	dw_2.Object.orpr_numero.Color	=	0
	
	dw_2.Object.clie_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.orpr_tipord.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.orpr_numero.BackGround.Color	=	RGB(255,255,255)
	
	dw_2.Object.b_orden.visible					=  1
ELSE
	dw_2.Object.clie_codigo.Protect				=	1
	dw_2.Object.orpr_tipord.Protect				=	1
	dw_2.Object.orpr_numero.Protect				=	1
	
	dw_2.Object.clie_codigo.Color		=	RGB(255,255,255)
	//dw_2.Object.orpr_tipord.Color		=	RGB(255,255,255)
	dw_2.Object.orpr_numero.Color	=	RGB(255,255,255)

	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
	dw_2.Object.orpr_tipord.BackGround.Color		=	553648127
	dw_2.Object.orpr_numero.BackGround.Color	=	553648127
	
	dw_2.Object.b_orden.visible					=  0
END IF
end subroutine

public function long buscabultos (string as_lote, integer ai_tipoenva, integer ai_enva);String   ls_Nombre, ls_envase, ls_codenva
Integer  li_tipodoc, li_orden, li_lotepl, li_lotesp, li_lote, li_tipomov, li_Cliente
Long     ll_Bultos, ll_numero

li_tipodoc	=	Integer(istr_mant.argumento[2])
li_orden		=	Integer(istr_mant.argumento[3])
li_Cliente	=	Integer(istr_mant.argumento[16])

li_lotepl   =	Integer(Mid(as_lote,1,4))
li_lotesp	=	Integer(mid(as_lote,5,2))
li_lote		=	Integer(mid(as_lote,7,10))

SELECT	distinct tpmv_codigo, mfge_numero
	INTO	:li_tipomov, :ll_numero
	FROM	dbo.spro_movtofrutagranenca
  WHERE	plde_codigo	=	:gstr_ParamPlanta.CodigoPlanta
    AND 	tpmv_codigo =  21
    AND  defg_tipdoc =  :li_Tipodoc
	 AND  defg_docrel =  :li_orden
	 AND  clie_codigo =  :li_Cliente; 
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento de Fruta Encabezado")

ELSEIF sqlca.SQLCode<>100 THEN
	
	 SELECT	mfgd_bulent
		INTO	:ll_Bultos
		FROM	dbo.spro_movtofrutagrandeta
  	  WHERE	plde_codigo	=	:gstr_ParamPlanta.CodigoPlanta
   	 AND  tpmv_codigo =  :li_tipomov
		 AND  mfge_numero =  :ll_numero
		 AND  lote_pltcod =  :li_lotepl
		 AND  lote_espcod	=	:li_lotesp
		 AND  lote_codigo	=	:li_lote
		 AND  enva_tipoen =  :ai_tipoenva
		 AND  enva_codigo =  :ai_enva
		 AND  clie_codigo =  :li_Cliente;
   
	IF sqlca.SQLCode = -1 THEN
	   F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento de Fruta Detalle")
   ELSEIF sqlca.SQLCode<>100 THEN
		
   END IF

END IF

Return ll_Bultos
end function

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
string   ls_loteco1, ls_tipoen1, ls_envase1, ls_planta1, ls_hora1
string   ls_loteco2, ls_tipoen2, ls_envase2, ls_planta2, ls_hora2

ls_loteco1 = string(dw_1.Object.lote_codigo[il_fila])
ls_planta1 = string(dw_1.Object.lote_pltcod[il_fila])
ls_tipoen1 = string(dw_1.Object.enva_tipoen[il_fila])
ls_envase1 = string(dw_1.Object.enva_codigo[il_fila])
ls_hora1   = string(dw_1.Object.opvd_horava[il_fila],"hh:mm")

CHOOSE CASE as_columna
	CASE "lote_codigo"
		ls_loteco1 = as_valor
	
	CASE "lote_pltcod"
		ls_planta1 = as_valor
		
	CASE "enva_tipoen"
		ls_tipoen1 = as_valor	
	
	CASE "enva_codigo"
		ls_envase1 = as_valor
		
	CASE "opvd_harava"
		ls_hora1 = MID(as_valor,12,5)	
		
END CHOOSE		

FOR ll_fila=1 To dw_1.Rowcount()
	IF ll_fila<>il_fila THEN
      ls_loteco2 = string(dw_1.Object.lote_codigo[ll_fila])
		ls_planta2 = string(dw_1.Object.lote_pltcod[ll_fila])
		ls_tipoen2 = string(dw_1.Object.enva_tipoen[ll_fila])
		ls_envase2 = string(dw_1.Object.enva_codigo[ll_fila])
		ls_hora2   = string(dw_1.Object.opvd_horava[ll_fila],"hh:mm")
	
		IF ls_loteco1 = ls_loteco2 AND ls_planta1 = ls_planta2 AND ls_tipoen1 = ls_tipoen2 AND &
		   ls_envase1 = ls_envase2 AND ls_hora1   = ls_hora2 THEN
			MessageBox("Error", "La hora de vaciado para el lote ya fue ingresada anteriormente", Information!, Ok!)
			RETURN True
		END IF	
	END IF
NEXT	

RETURN False

end function

public function boolean habilitabultos (string as_columna, string as_valor);//Boolean	lb_Estado = True
//Long     ll_Bultos, ll_bultosrep, ll_bultosyaingresados
//String   ls_lote, ls_null
//Integer  li_tipo, li_enva
//
//SetNull(ls_Null)
// 
//IF as_Columna <> "lote" AND &
//	((dw_1.Object.lote[il_fila] = "") OR isnull(dw_1.Object.lote[il_fila])) THEN
//	lb_Estado = False
//END IF
//	
//IF as_Columna <> "enva_tipoen" AND &
//	((dw_1.Object.enva_tipoen[il_fila] = 0) OR (IsNull(dw_1.Object.enva_tipoen[il_fila]))) THEN
//	lb_Estado = False
//END IF
//	
//IF as_Columna <> "enva_codigo" AND &
//	(dw_1.Object.enva_codigo[il_fila] = 0 OR IsNull(dw_1.Object.enva_codigo[il_fila])) THEN
//	lb_Estado = False
//END IF
//
//IF lb_estado THEN
//	ls_lote	=	dw_1.Object.lote[il_fila]
//	li_tipo	=	dw_1.Object.enva_tipoen[il_fila]
//	li_enva	=	dw_1.Object.enva_codigo[il_fila]
//	
//	IF as_columna="lote" 		 THEN ls_lote	=	as_valor
//	IF as_columna="enva_tipoen" THEN li_tipo	=	integer(as_valor)
//	IF as_columna="enva_codigo" THEN li_enva	=	integer(as_valor)
//	
//	IF ls_lote<>"" AND isnull(li_tipo)=FALSE AND isnull(li_enva)=FALSE THEN
//		
//		ll_Bultos=BuscaBultos(ls_lote,li_tipo, li_enva)
//		
//		IF ll_Bultos<= 0 THEN
//			MessageBox("Atención","Número de Lote Ingresado No posee Bultos de Movimiento. Debe Ingresar Otro.")
//			dw_1.Object.lote[il_fila]				=	ls_null
//			dw_1.Object.lote_codigo[il_fila]    =  integer(ls_null)
//			dw_1.Object.enva_tipoen[il_fila]		=	Integer(ls_null)
//			dw_1.Object.enva_codigo[il_fila]		=	Integer(ls_null)
//			dw_1.Object.enva_nombre[il_fila]		=	ls_null
//			dw_1.Object.cale_calida[il_fila]		=	ls_null
//			dw_1.Object.cale_nombre[il_fila]		=	ls_null
//			dw_1.Object.opvd_canbul[il_fila]		=	long(ls_null)
//			dw_1.Object.bultostot[il_fila]		=	long(ls_null)
//			
//			RETURN FALSE
//		ELSE	
//			IF dw_1.RowCount()>1 THEN			  
//				ll_bultosrep = buscalotesrepetidos(ls_lote,li_tipo,li_enva,il_fila)
//			ELSE	 
//				ll_bultosrep = 0
//			END IF	 
//			
//		   ll_bultosyaingresados = buscabultosyaingresados(ls_lote,li_tipo,li_enva)
//			
//		   IF isnull(ll_bultosyaingresados) THEN ll_bultosyaingresados = 0
//			
//			ll_bultos = ll_bultos - ll_bultosyaingresados - ll_bultosrep
//			IF ll_bultos<=0 THEN
//				MessageBox("Atención","El lote ya fue ingresado en su totalidad. Debe Ingresar Otro.")
//				dw_1.Object.lote[il_fila]				=	ls_null
//				dw_1.Object.lote_codigo[il_fila]    =  integer(ls_null)
//				dw_1.Object.enva_tipoen[il_fila]		=	Integer(ls_null)
//				dw_1.Object.enva_codigo[il_fila]		=	Integer(ls_null)
//				dw_1.Object.enva_nombre[il_fila]		=	ls_null
//				dw_1.Object.cale_calida[il_fila]		=	ls_null
//				dw_1.Object.cale_nombre[il_fila]		=	ls_null
//				dw_1.Object.opvd_canbul[il_fila]		=	long(ls_null)
//				dw_1.Object.bultostot[il_fila]		=	long(ls_null)
//				dw_1.Object.enva_codigo[il_fila]		=	Integer(ls_null)
//				
//				RETURN FALSE
//				
//			ELSE
//				dw_1.Object.opvd_canbul[il_fila] 	=	ll_Bultos
//				dw_1.Object.bultostot[il_fila]   	=	0      //ll_bultos 
//				actualizatotalbultos(ls_lote,li_tipo, li_enva,il_fila,0)
//			END IF
//      END IF			
//	END IF	
//END IF	
//
RETURN TRUE
end function

public subroutine buscaorden ();Str_Busqueda	lstr_busq
String ls_Nula
SetNull(ls_nula)

lstr_busq.argum[1]	=	istr_mant.argumento[1]
lstr_busq.argum[2]	=	"-1"
lstr_busq.argum[3]	=  istr_mant.argumento[2]
lstr_busq.argum[4]	=  istr_mant.argumento[16]

OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" and lstr_busq.argum[1]<> "0" THEN
	
	IF iuo_ordenproceso.Existe(gstr_ParamPlanta.CodigoPlanta,&
											Integer(istr_Mant.Argumento[2]), &
											Integer(lstr_busq.argum[6]),True,Sqlca, &
											Integer(istr_mant.argumento[16])) THEN	
											
		istr_mant.argumento[3]	= lstr_busq.argum[6]
		dw_2.SetItem(1,"orpr_numero",long(lstr_busq.argum[6]))
		Habilitadetalle("orpr_numero")
		TriggerEvent("ue_recuperadatos")			 	
	ELSE
		dw_2.SetItem(1,"orpr_numero",long(ls_Nula))
	END IF
END IF
end subroutine

public subroutine habilitadetalle (string as_columna);Boolean	lb_Estado = True
Date     ldt_fecha, ld_fechavac
Integer  li_planta, li_tipord, li_turnovac, li_Cliente
Long     ll_orden

li_Cliente	=	Integer(istr_mant.argumento[16])

IF as_Columna <> "orpr_numero" AND &
	(dw_2.Object.orpr_numero[1] = 0 OR IsNull(dw_2.Object.orpr_numero[1])) THEN
	lb_Estado = False
END IF
	
IF lb_estado THEN
  pb_ins_det.Enabled = lb_estado
  dw_2.accepttext()
  li_planta 	=	dw_2.Object.plde_codigo[1]
  li_tipord 	=	dw_2.Object.orpr_tipord[1]
  ll_orden		=	dw_2.Object.orpr_numero[1]
END IF	

end subroutine

public function boolean noexistecliente (integer al_codigo);Integer	li_cliente

SELECT clie_codigo
INTO	:li_cliente
FROM	dbo.clientesprod
WHERE	clie_codigo	=	:al_codigo;
	
IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla ClientesProd" )
		Return True			
ELSEIF sqlca.SQLCode = 100 THEN
		messagebox("Atención","No Existe Código Seleccionado en Tabla ")			
		Return True						
END IF

Return False

end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

//IF Not dw_2.uf_check_required(0) THEN RETURN False

//IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
				Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
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

on w_maed_spro_pesajepallets.create
call super::create
end on

on w_maed_spro_pesajepallets.destroy
call super::destroy
end on

event open;/* 
Argumentos
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Tipo Orden
istr_Mant.Argumento[3]	=	Numero
istr_Mant.Argumento[4]	=	Fecha Vaciado
istr_Mant.Argumento[5]	=	Turno
istr_Mant.Argumento[6]	=	Estado Orden de Vaciado
istr_Mant.Argumento[7]	=	
istr_Mant.Argumento[16]	=	Cliente
*/

x				= 0
y				= 0

This.Height	= 2500
im_menu		= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	"4"
istr_Mant.Argumento[3]	=	""
istr_Mant.Argumento[4]	=	""
istr_Mant.Argumento[5]	=	""
istr_Mant.Argumento[6]	=	"V"
istr_Mant.Argumento[16]	=	String(gi_Codexport)

dw_2.Object.clie_codigo[1] =  Integer(istr_mant.argumento[16])

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(gi_codexport)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta =	False

pb_nuevo.PostEvent(Clicked!)

iuo_LineaPacking	=	Create uo_lineapacking
iuo_ordenproceso	=	Create uo_spro_ordenproceso
iuo_plantalote    =	Create uo_plantadesp

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
			ll_modif1  +=	dw_2.GetNextModified(0, Primary!)
		
			IF dw_1.RowCount() > 0 ANd ll_modif1 > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","¿Desea Grabar la información?", Question!, YesNoCancel!)
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
dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.SetRedraw(True)
dw_2.InsertRow(0)
dw_2.SetFocus()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True


dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.orpr_tipord[1]		=	4
dw_2.Object.clie_codigo[1] 	=  Integer(istr_mant.argumento[16])

istr_mant.argumento[2] 		=	"4"
istr_mant.argumento[3]		=	""

habilitaencab(TRUE)
dw_2.SetColumn("orpr_numero")
end event

event ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_row, ll_bultos, ll_bultosrep, ll_bultosyaingresados
Date		ld_fecha
Integer  li_Protec=0
String   ls_lote

ld_Fecha	=	Date(left(istr_Mant.Argumento[4], 10))

DO
	dw_2.SetRedraw(False)
	
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_mant.Argumento[1]), &
										  Integer(istr_mant.Argumento[16]), &
										  Integer(istr_mant.Argumento[2]), &
										  Integer(istr_mant.Argumento[3]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	=	dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
										  Integer(istr_mant.Argumento[16]), &
										  Integer(istr_mant.Argumento[2]), &
										  Integer(istr_mant.Argumento[3]))
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				IF dw_1.RowCount() > 1 THEN
					pb_eli_det.Enabled	=	TRUE
				ELSE
					pb_eli_det.Enabled	=	FALSE
					pb_grabar.Enabled		=	TRUE
					pb_eliminar.Enabled	=	TRUE
					pb_imprimir.Enabled	=	True
				
					HabilitaEncab(False)
					pb_ins_det.SetFocus()
				END IF
			END IF
				dw_2.SetRedraw(True)
		LOOP WHILE respuesta = 1
		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)


end event

event ue_nuevo_detalle;
IF il_fila > 0 THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled		= True
	pb_eli_det.Enabled = True
END IF

Istr_mant2.dw 					= dw_1
istr_mant2.Argumento[1]	= 	String(dw_2.Object.clie_codigo[1])
istr_mant2.Argumento[2]	= 	String(dw_2.Object.plde_codigo[1])
istr_mant2.Argumento[3]	= 	String(dw_2.Object.orpr_tipord[1])
istr_mant2.Argumento[4]	= 	String(dw_2.Object.orpr_numero[1])
istr_mant2.Argumento[6]	=	String(dw_1.RowCount())
istr_mant2.Argumento[8]	=	"0"
istr_mant2.Argumento[9]	=	String(dw_1.GetRow())
istr_mant2.agrega				= True
OpenWithParm(iw_detalles, istr_mant2)
end event

event ue_modifica_detalle;call super::ue_modifica_detalle;istr_mant2.agrega	= False
Istr_mant2.dw 					= dw_1
istr_mant2.Argumento[1]	= 	String(dw_2.Object.clie_codigo[1])
istr_mant2.Argumento[2]	= 	String(dw_2.Object.plde_codigo[1])
istr_mant2.Argumento[3]	= 	String(dw_2.Object.orpr_tipord[1])
istr_mant2.Argumento[4]	= 	String(dw_2.Object.orpr_numero[1])
istr_mant2.Argumento[6]	=	String(dw_1.RowCount())
istr_mant2.Argumento[8]	=	"1"
istr_mant2.Argumento[9]	=	String(dw_1.GetRow())
OpenWithParm(iw_detalles, istr_mant2)

end event

event ue_borra_detalle;call super::ue_borra_detalle;Long ll_bultos
String ls_lote
Integer  li_tipen, li_envas

IF istr_Mant.Solo_Consulta THEN RETURN

IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
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
	END IF
END IF
end event

event ue_antesguardar;Long ll_fila, ll_row, ll_paso, suma, bultostotal
Integer li_lotepl, li_lotesp, li_lote, li_Contador
Boolean lb_pasada=True
String	ls_Mensaje, ls_Columna[], ls_fecha, ls_hora

Message.DoubleParm = 1

IF dw_1.RowCount()<1 THEN
	MessageBox("Error de Detalle", "No ha Ingresado Detalle.")
	Message.DoubleParm = -1
	RETURN
END IF

FOR ll_Fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_1.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_1.Object.orpr_tipord[ll_Fila]	=	dw_2.Object.orpr_tipord[1]
		dw_1.Object.orpr_numero[ll_Fila]	=	dw_2.Object.orpr_numero[1]
		dw_1.Object.clie_codigo[ll_Fila]	=	dw_2.Object.clie_codigo[1]
		il_Fila = ll_Fila
	END IF
NEXT



end event

event ue_seleccion;call super::ue_seleccion;Str_Busqueda	lstr_busq
String ls_Nula
SetNull(ls_nula)

lstr_busq.argum[1]	=	istr_mant.argumento[1]
lstr_busq.argum[2]	=	istr_mant.argumento[2]
lstr_busq.argum[3]	=	istr_mant.argumento[3]
lstr_busq.argum[4]	=	istr_mant.argumento[16]

OpenWithParm(w_busc_spro_ordenvaciado, lstr_busq)
lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" and lstr_busq.argum[1]<> "0" THEN
	dw_2.Object.plde_codigo[1] = integer(lstr_busq.argum[1])
	dw_2.Object.orpr_tipord[1] = integer(lstr_busq.argum[2])
	
	IF iuo_ordenproceso.Existe(integer(lstr_busq.argum[1]),&
										Integer(lstr_busq.argum[2]), &
										Integer(lstr_busq.argum[3]),True,Sqlca,&
										Integer(istr_mant.argumento[16])) THEN	
									
	istr_mant.argumento[1]	= lstr_busq.argum[1]
	istr_mant.argumento[2]	= lstr_busq.argum[2]
	istr_mant.argumento[3]	= lstr_busq.argum[3]
	istr_mant.argumento[4]	= lstr_busq.argum[4]
	istr_mant.argumento[5]	= lstr_busq.argum[5]
	
	TriggerEvent("ue_recuperadatos")
		
	ELSE
		dw_2.SetItem(1,"orpr_numero",long(ls_Nula))
	END IF
END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
Date ld_fecha

istr_info.titulo	= "CONFIRMACION DE VACIADO"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_pesaje_pallets"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[16]), &
								  Integer(istr_mant.Argumento[1]), &
								  Integer(istr_mant.Argumento[2]), &
								  Integer(istr_mant.Argumento[3]))

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

event ue_validaborrar;
IF MessageBox("Borrar Registro","Desea Borrar la Información ?", Question!, YesNo!) = 1 THEN
	Message.DoubleParm = 1
ELSE
	Message.DoubleParm = -1
END IF

RETURN
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_pesajepallets
integer x = 416
integer y = 348
integer width = 1458
integer height = 1380
string title = "Detalle de Pesaje de Pallets"
string dataobject = "dw_mues_deta_pesajepallets"
end type

event dw_1::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event dw_1::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event dw_1::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event type long dw_1::ue_seteafila(unsignedlong wparam, long lparam);il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, True)

RETURN 0
end event

event type long dw_1::dwnkey(keycode key, unsignedlong keyflags);il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_spro_ordenprocvaciado.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_spro_ordenprocvaciado.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event dw_1::constructor;call super::constructor;This.Uf_add_validation('opvd_canbul > 0 and opvd_canbul <= 9999','Valores fuera de rango para cantidad de bultos')
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_pesajepallets
integer x = 59
integer y = 40
integer width = 2103
integer height = 256
string dataobject = "dw_mues_enca_pesajepallets"
end type

event dw_2::itemchanged;Long		ll_null
Integer	li_codigo, li_null
String	ls_columna
Date     ld_fecha

SetNull(ll_null)
SetNull(li_null)

ls_columna = dwo.Name

CHOOSE CASE ls_columna
		
	CASE "clie_codigo"
		
			IF NoExisteCliente(Integer(Data)) THEN
				This.SetItem(row, ls_Columna, li_Null)
				RETURN 1
			END IF	
			
			istr_mant.argumento[16]	=	data
		
	CASE "orpr_numero"	
		
		IF iuo_ordenproceso.Existe(gstr_ParamPlanta.CodigoPlanta,&
											Integer(istr_Mant.Argumento[2]), &
											Long(data),True,Sqlca,&
											Integer(istr_mant.argumento[16])) THEN	
											
			istr_mant.argumento[3]	= data
			parent.TriggerEvent("ue_recuperadatos")
			dw_2.Object.orpr_tipord.Color		=	RGB(0,0,0)
			dw_2.Object.orpr_tipord.BackGround.Color		=	553648127
			pb_grabar.Enabled = true
			pb_imprimir.Enabled = true
		ELSE
			dw_2.SetItem(1,"orpr_numero",ll_Null)
			RETURN 1
		END IF
		
	CASE "orpr_tipord"	
		istr_mant.argumento[2] = Data
		dw_2.SetItem(1,"orpr_numero",ll_Null)
		

		
END CHOOSE

Habilitadetalle(ls_columna)

end event

event dw_2::buttonclicked;call super::buttonclicked;string ls_columna

ls_columna = dwo.Name

CHOOSE CASE ls_columna		
	CASE "b_orden"
		buscaorden()
		
END CHOOSE		
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_pesajepallets
integer x = 2395
integer y = 284
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_pesajepallets
integer x = 2395
integer y = 464
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_pesajepallets
integer x = 2395
integer y = 648
integer weight = 400
fontcharset fontcharset = ansi!
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_pesajepallets
integer x = 2395
integer y = 824
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_pesajepallets
integer x = 2395
integer y = 1004
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_pesajepallets
integer x = 2395
integer y = 1392
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_pesajepallets
integer x = 2395
integer y = 1564
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_pesajepallets
integer x = 2395
integer y = 104
end type

