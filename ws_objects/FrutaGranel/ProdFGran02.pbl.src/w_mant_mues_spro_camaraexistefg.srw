$PBExportHeader$w_mant_mues_spro_camaraexistefg.srw
forward
global type w_mant_mues_spro_camaraexistefg from w_mant_directo
end type
type dw_2 from datawindow within w_mant_mues_spro_camaraexistefg
end type
type cb_aplica from commandbutton within w_mant_mues_spro_camaraexistefg
end type
type dw_4 from datawindow within w_mant_mues_spro_camaraexistefg
end type
type dw_5 from datawindow within w_mant_mues_spro_camaraexistefg
end type
type dw_3 from datawindow within w_mant_mues_spro_camaraexistefg
end type
end forward

global type w_mant_mues_spro_camaraexistefg from w_mant_directo
integer width = 3653
integer height = 2336
string title = "Traslado de Fruta Granel en Camara"
dw_2 dw_2
cb_aplica cb_aplica
dw_4 dw_4
dw_5 dw_5
dw_3 dw_3
end type
global w_mant_mues_spro_camaraexistefg w_mant_mues_spro_camaraexistefg

type variables
uo_plantadesp			iuo_plantadesp
uo_camarasfrigo		iuo_camarasfrigoorig
uo_camarasfrigo		iuo_camarasfrigodest
uo_lotesfrutagranel	iuo_Lotes
uo_cliente				iuo_clientes
uo_tipomovtofruta		iuo_tipomovtofruta

DataWindowChild		idwc_planta, idwc_camaraorig, idwc_camaradest, idwc_especie, idwc_cliente, &
							idwc_cam01, idwc_cam02

Integer					ii_planta, ii_tipomovto, ii_cliente
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
public subroutine actualizamovto ()
public subroutine buscafolio ()
public function long duplicado (string planta, string tipoenva, string especie, string envase, string lote)
public function boolean existefolio (long al_folio)
public subroutine habilitaencab (boolean habilita)
public function boolean lotesdestarados (string as_lote)
end prototypes

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_5.Update(True, False) > -1 THEN
	IF dw_3.Update(True, False) > -1 THEN
		IF dw_1.Update(True, False) > -1 THEN
	
			Commit;
		
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				lb_Retorno	=	False
			ELSE
				lb_Retorno	=	True
					
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
				dw_3.ResetUpdate()
				dw_5.ResetUpdate()
			END IF
		ELSE
			RollBack;
			
			IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
			
			lb_Retorno	=	False
		END IF
	ELSE
		RollBack;
		
		IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
		
		lb_Retorno	=	False
	END IF

ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF
	
sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno


end function

public subroutine actualizamovto ();Long ll_fila2, ll_FilaNueva , ll_fila1, ll_fila, ll_tobul, ll_filamov
string ls_lotepl, ls_lotees, ls_loteco, ls_tipoen, ls_envase

FOR ll_fila1 = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila1, 0, Primary!) = NewModified! OR &
		dw_1.GetItemStatus(ll_fila1, 0, Primary!) = DataModified!THEN
		
		   ls_lotepl = string(dw_1.Object.lote_pltcod[ll_fila1])
			ls_lotees = string(dw_1.Object.lote_espcod[ll_fila1])
			ls_loteco = string(dw_1.Object.lote_codigo[ll_fila1])
			ls_tipoen = string(dw_1.Object.enva_tipoen[ll_fila1])
			ls_envase = string(dw_1.Object.enva_codigo[ll_fila1])
			
			ll_filaMov	= dw_3.Find("lote_pltcod =" + ls_lotepl + " AND enva_tipoen =" + ls_tipoen + " AND " + &
										   "lote_espcod =" + ls_lotees + " AND enva_codigo =" + ls_envase + " AND " + &
										   "lote_codigo =" + ls_loteco + " AND mvca_tipomv = 1", 1, dw_3.RowCount())

			IF ll_FilaMov = 0 THEN
				ll_FilaNueva = dw_3.InsertRow(0)
				dw_3.Object.lote_pltcod[ll_FilaNueva]  = dw_1.Object.lote_pltcod[ll_fila1]
				dw_3.Object.lote_espcod[ll_FilaNueva]  = dw_1.Object.lote_espcod[ll_fila1]
				dw_3.Object.lote_codigo[ll_FilaNueva]  = dw_1.Object.lote_codigo[ll_fila1]
				dw_3.Object.mvca_tipomv[ll_FilaNueva]	= 1
				dw_3.Object.cama_codigo[ll_FilaNueva]	= Integer(istr_Mant.Argumento[4])
				dw_3.Object.mvca_nroban[ll_FilaNueva]	= dw_1.Object.caex_nroban[ll_fila1]
				dw_3.Object.mvca_nropos[ll_FilaNueva]	= dw_1.Object.caex_nropos[ll_fila1]
				dw_3.Object.mvca_nropis[ll_FilaNueva]	= dw_1.Object.caex_nropis[ll_fila1]			
				dw_3.Object.enva_tipoen[ll_FilaNueva]  = dw_1.Object.enva_tipoen[ll_fila1]
				dw_3.Object.enva_codigo[ll_FilaNueva]  = dw_1.Object.enva_codigo[ll_fila1]
				dw_3.Object.enva_nombre[ll_FilaNueva]  = dw_1.Object.enva_nombre[ll_fila1]
				dw_3.Object.mvca_canbul[ll_FilaNueva]	= dw_1.Object.caex_canbul[ll_fila1]
				dw_3.SetItemStatus(ll_filaNueva, 0,  Primary!, NewModified!)
			ELSEIF ll_FilaMov>0 THEN
				dw_3.Object.mvca_nroban[ll_FilaMov]	= dw_1.Object.caex_nroban[ll_fila1]
				dw_3.Object.mvca_nropos[ll_FilaMov]	= dw_1.Object.caex_nropos[ll_fila1]
				dw_3.Object.mvca_nropis[ll_FilaMov]	= dw_1.Object.caex_nropis[ll_fila1]			
				dw_3.Object.mvca_canbul[ll_FilaMov]	= dw_1.Object.caex_canbul[ll_fila1]
				dw_3.Object.mvca_canbul[ll_FilaMov]	= ll_tobul
			END IF	
	END IF
NEXT

FOR ll_fila2 = 1 TO dw_2.RowCount()
	IF dw_2.GetItemStatus(ll_fila2, 0, Primary!) = DataModified! THEN
      ls_lotepl = string(dw_2.Object.lote_pltcod[ll_fila2])
		ls_lotees = string(dw_2.Object.lote_espcod[ll_fila2])
		ls_loteco = string(dw_2.Object.lote_codigo[ll_fila2])
		ls_tipoen = string(dw_2.Object.enva_tipoen[ll_fila2])
		ls_envase = string(dw_2.Object.enva_codigo[ll_fila2])
		
		ll_filaMov	= dw_3.Find("lote_pltcod =" + ls_lotepl + " AND enva_tipoen =" + ls_tipoen + " AND " + &
		 								"lote_espcod =" + ls_lotees + " AND enva_codigo =" + ls_envase + " AND " + &
										"lote_codigo =" + ls_loteco + " AND mvca_tipomv = 2", 1, dw_3.RowCount())
		IF ll_FilaMov=0 THEN								
			ll_FilaNueva = dw_3.InsertRow(0)
			dw_3.Object.lote_pltcod[ll_FilaNueva]  = dw_2.Object.lote_pltcod[ll_fila2]
			dw_3.Object.lote_espcod[ll_FilaNueva]  = dw_2.Object.lote_espcod[ll_fila2]
			dw_3.Object.lote_codigo[ll_FilaNueva]  = dw_2.Object.lote_codigo[ll_fila2]
			dw_3.Object.mvca_tipomv[ll_FilaNueva]	= 2
			dw_3.Object.cama_codigo[ll_FilaNueva]	= Integer(istr_Mant.Argumento[3])
			dw_3.Object.mvca_nroban[ll_FilaNueva]	= dw_2.Object.caex_nroban[ll_fila2]
			dw_3.Object.mvca_nropos[ll_FilaNueva]	= dw_2.Object.caex_nropos[ll_fila2]
			dw_3.Object.mvca_nropis[ll_FilaNueva]	= dw_2.Object.caex_nropis[ll_fila2]
			ll_fila =  dw_1.Find("lote_pltcod =" + ls_lotepl + " AND enva_tipoen =" + ls_tipoen + " AND " + &
		 								"lote_espcod =" + ls_lotees + " AND enva_codigo =" + ls_envase + " AND " + &
										"lote_codigo =" + ls_loteco , 1, dw_1.RowCount())
			IF ll_fila>0 THEN							
			   dw_3.Object.mvca_canbul[ll_FilaNueva]	= dw_1.Object.tot_bultos[ll_fila]
			ELSE
				dw_3.Object.mvca_canbul[ll_FilaNueva]	= 0
			END IF	
			dw_3.Object.enva_tipoen[ll_FilaNueva]  = dw_2.Object.enva_tipoen[ll_fila2]
			dw_3.Object.enva_codigo[ll_FilaNueva]  = dw_2.Object.enva_codigo[ll_fila2]
			dw_3.Object.enva_nombre[ll_FilaNueva]  = dw_2.Object.enva_nombre[ll_fila2]
			dw_3.Object.tien_nombre[ll_FilaNueva]  = dw_2.Object.tien_nombre[ll_fila2]
		ELSEIF ll_FilaMov>0 THEN
			dw_3.Object.mvca_nroban[ll_FilaMov]	= dw_1.Object.caex_nroban[ll_fila2]
			dw_3.Object.mvca_nropos[ll_FilaMov]	= dw_1.Object.caex_nropos[ll_fila2]
			dw_3.Object.mvca_nropis[ll_FilaMov]	= dw_1.Object.caex_nropis[ll_fila2]			
			ll_fila =  dw_1.Find("lote_pltcod =" + ls_lotepl + " AND enva_tipoen =" + ls_tipoen + " AND " + &
		 								"lote_espcod =" + ls_lotees + " AND enva_codigo =" + ls_envase + " AND " + &
										"lote_codigo =" + ls_loteco , 1, dw_1.RowCount())
			IF ll_fila>0 THEN							
				ll_tobul = dw_1.Object.tot_bultos[ll_fila]+dw_3.Object.mvca_canbul[ll_FilaMov]
			ELSE
				ll_tobul = dw_3.Object.mvca_canbul[ll_FilaMov]
			END IF	
			dw_3.Object.mvca_canbul[ll_FilaMov]	= ll_tobul			
		END IF	
	END IF
NEXT

end subroutine

public subroutine buscafolio ();Str_busqueda	lstr_busq

lstr_busq.argum[1] = String(dw_5.Object.plde_codigo[1])
lstr_busq.argum[2] = String(dw_5.Object.mvce_fecmov[1])

OpenWithParm(w_busc_movtocamaenca, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[3] = "" THEN
	dw_5.SetColumn("mvce_numero")
	dw_5.SetFocus()
ELSE
	dw_5.Object.mvce_numero[1]	=	Long(lstr_busq.argum[3])
	istr_mant.argumento[6] 		=	lstr_busq.argum[3]
	triggerevent("ue_recuperadatos")

END IF

RETURN
end subroutine

public function long duplicado (string planta, string tipoenva, string especie, string envase, string lote);Long		ll_Fila
String	ls_codigo

ll_fila	= dw_1.Find("lote_pltcod =" + planta + " AND " + "enva_tipoen =" + TipoEnva + " AND " + &
							"lote_espcod =" + especie+ " AND " + "enva_codigo =" + Envase   + " AND " + &
							"lote_codigo = " + lote, 1, dw_1.RowCount())

RETURN ll_Fila



end function

public function boolean existefolio (long al_folio);Long ll_Folio
Integer li_planta

li_planta = dw_5.Object.plde_codigo[1]

SELECT mvce_numero INTO :ll_Folio
  FROM dbo.spro_movtocamarafgenca
 WHERE plde_codigo = :li_planta
   AND tpmv_codigo = 81
   AND mvce_numero = :al_folio;
		 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Movimiento de Camara Encabezado" )
	RETURN FALSE	
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","El Número de Folio no Existe. Ingrese Otro Folio.")
	RETURN FALSE
END IF
		 
RETURN TRUE		 
end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_5.Object.mvce_numero.Protect 	= 0
	dw_5.Object.mvce_fecmov.Protect 	= 0
	dw_5.Object.espe_codigo.Protect 		= 0
	dw_5.Object.cama_codori.Protect 		= 0
	dw_5.Object.cama_coddes.Protect	= 0

	dw_5.Object.mvce_numero.Color	=	0
	dw_5.Object.mvce_fecmov.Color	=	0
	dw_5.Object.espe_codigo.Color	=	0
	dw_5.Object.cama_codori.Color	=	0
	dw_5.Object.cama_coddes.Color	=	0
	
	dw_5.Object.mvce_numero.BackGround.Color	=	RGB(255,255,255)
	dw_5.Object.mvce_fecmov.BackGround.Color	=	RGB(255,255,255)
	dw_5.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_5.Object.cama_codori.BackGround.Color	=	RGB(255,255,255)
	dw_5.Object.cama_coddes.BackGround.Color	=	RGB(255,255,255)
	
	dw_5.Object.b_folio.visible					=  1
	
ELSE
	dw_5.Object.mvce_numero.Protect	= 1
	dw_5.Object.mvce_fecmov.Protect 	= 1
	dw_5.Object.espe_codigo.Protect 		= 1
	dw_5.Object.cama_codori.Protect 		= 1
	dw_5.Object.cama_coddes.Protect 	= 1

	dw_5.Object.mvce_numero.Color	=	RGB(255,255,255)
	dw_5.Object.mvce_fecmov.Color	=	RGB(255,255,255)
	dw_5.Object.espe_codigo.Color	=	RGB(255,255,255)
	dw_5.Object.cama_codori.Color	=	RGB(255,255,255)
	dw_5.Object.cama_coddes.Color	=	RGB(255,255,255)
	
	dw_5.Object.mvce_numero.BackGround.Color	=	553648127
	dw_5.Object.mvce_fecmov.BackGround.Color	=	553648127
	dw_5.Object.espe_codigo.BackGround.Color	=	553648127
	dw_5.Object.cama_codori.BackGround.Color	=	553648127
	dw_5.Object.cama_coddes.BackGround.Color	=	553648127
	
	dw_5.Object.b_folio.visible					=  0
END IF
end subroutine

public function boolean lotesdestarados (string as_lote);Integer li_codigo, li_plt, li_esp, li_lot
String  ls_nombre
Boolean lb_Retorno=True

li_plt = Integer(Mid(as_lote,1,4))
li_esp = Integer(Mid(as_lote,5,2))
li_lot = Integer(Mid(as_lote,7,4))

SELECT lote_codigo
  INTO :li_codigo
  FROM dbo.spro_lotesfrutagranel
 WHERE lote_pltcod = :li_plt
	AND lote_espcod = :li_esp
	AND lote_codigo = :li_lot
	AND lote_totnet > 0;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Lotes Fruta Granel")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	lb_Retorno	=	False
END IF	
RETURN lb_retorno

end function

on w_mant_mues_spro_camaraexistefg.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.cb_aplica=create cb_aplica
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.cb_aplica
this.Control[iCurrent+3]=this.dw_4
this.Control[iCurrent+4]=this.dw_5
this.Control[iCurrent+5]=this.dw_3
end on

on w_mant_mues_spro_camaraexistefg.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.cb_aplica)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_3)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_Fila, Respuesta, ll_Filaori, ll_Fila3
Integer  li_especie

IF istr_mant.Argumento[6] <> "" THEN
	DO
		ll_Fila	= dw_5.Retrieve(Integer(istr_Mant.Argumento[2]), ii_cliente,  &
										 Integer(istr_Mant.Argumento[6]))
	  IF ll_Fila = -1 THEN
			Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	  ELSEIF ll_Fila > 0 THEN
			ll_Filaori	= dw_3.Retrieve(Integer(istr_Mant.Argumento[2]), ii_cliente, &
												 Integer(istr_Mant.Argumento[6]))
	
			IF ll_Filaori = -1 THEN
				Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
													Information!, RetryCancel!)
			
			END IF
			
			IF dw_3.RowCount()>0 THEN
				li_especie = dw_3.Object.lote_espcod[1]
				dw_5.Object.espe_codigo[1]= li_especie
			END IF
			
			HabilitaEncab(False)
			dw_1.visible=False
			dw_2.visible=False
			dw_3.visible=TRUE
			cb_aplica.visible = FALSE
			pb_lectura.Enabled   = FALSE
			pb_insertar.Enabled	= FALSE
			pb_eliminar.Enabled	= FALSE
			pb_grabar.Enabled		= FALSE
			pb_imprimir.Enabled	= True
		ELSE
			MessageBox(	"Error de Consistencia", "Camara de Origen sin Datos.", &
							Information!)
		END IF
	
	LOOP WHILE Respuesta = 1
ELSE
	DO
		dw_2.SetRedraw(FALSE)
		dw_1.SetRedraw(FALSE)
		ll_Fila	= dw_2.Retrieve(Integer(istr_Mant.Argumento[2]), -1, -1, 'CAM', &
										 Integer(istr_Mant.Argumento[3]))
	
		IF ll_Fila = -1 THEN
			Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
		ELSEIF ll_Fila > 0 THEN
	
						ll_Filaori	= dw_1.Retrieve(Integer(istr_Mant.Argumento[2]), -1, -1, 'CAM', &
															 Integer(istr_Mant.Argumento[4]))
			
						IF ll_Filaori = -1 THEN
							Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
															Information!, RetryCancel!)
					
						END IF
			dw_2.SetFilter("lote_espcod = " +string(dw_5.Object.espe_codigo[1]))
			dw_2.Filter()
			dw_1.SetFilter("lote_espcod = " +string(dw_5.Object.espe_codigo[1]))
			dw_1.Filter()
			HabilitaEncab(False)
	
			dw_1.SetRow(1)
			dw_1.SetFocus()
			cb_aplica.visible 	= TRUE
			pb_insertar.Enabled	= True
			pb_eliminar.Enabled	= True
			pb_grabar.Enabled		= True
			dw_1.Enabled = True
			//dw_2.SetColumn("tot_bultos")
		ELSE
			MessageBox(	"Error de Consistencia", "Camara de Origen sin Datos.", &
							Information!)
			pb_insertar.Enabled	= True
			pb_insertar.SetFocus()
		END IF
		dw_2.SetRedraw(TRUE)
		dw_1.SetRedraw(TRUE)
	LOOP WHILE Respuesta = 1
	
	IF Respuesta = 2 THEN Close(This)
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_2.SetColumn("tot_bultos")

dw_5.Object.clie_codigo[1]				=	gi_codexport
end event

event ue_imprimir;SetPointer(HourGlass!)


Integer	li_Planta
Long		Fila, ll_Folio
Integer  li_Tipmov

str_info	lstr_info

lstr_info.titulo	= "INFORME TRASLADO DE CAMARAS"
lstr_info.copias	= 1

OpenWithParm(vinf, lstr_info) 

vinf.dw_1.DataObject = "dw_info_spro_movtocamarafgenca"
vinf.dw_1.SetTransObject(sqlca)

li_Planta		=	dw_5.Object.plde_codigo[1]
li_tipmov      =  81
ll_Folio			=	dw_5.Object.mvce_numero[1]

Fila = vinf.dw_1.Retrieve(li_Planta,li_Tipmov, ll_Folio)

IF Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.",StopSign!, Ok!)
ELSE	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
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

event open;/*
Argumentos
*********************************************
istr_Mant.Argumento[1]	=> Tipo de Movimiento
istr_Mant.Argumento[2]	=> Código de Planta
istr_Mant.Argumento[3]	=> Código de Camara Origen
istr_Mant.Argumento[4]	=> Código de Camara Destino.
*********************************************
*/

x				= 0
y				= 0

This.Height	= 2500
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

//Cliente
dw_5.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve()
dw_5.Object.clie_codigo[1]				=	gi_codexport

//Planta
dw_5.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.insertRow(0)
END IF

//Especie
dw_5.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(SqlCa)
IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_especie.insertRow(0)
END IF

//Camara Origen
dw_5.GetChild("cama_codori", idwc_camaraorig)
idwc_camaraorig.SetTransObject(SqlCa)
IF idwc_camaraorig.Retrieve(gstr_paramplanta.codigoplanta) = 0 THEN
	MessageBox("Atención","Falta Registrar Camaras")
	idwc_camaraorig.InsertRow(0)
END IF

//Camara Destino
dw_5.GetChild("cama_coddes", idwc_camaradest)
idwc_camaradest.SetTransObject(SqlCa)
IF idwc_camaradest.Retrieve(gstr_paramplanta.codigoplanta) = 0 THEN
	MessageBox("Atención","Falta Registrar Camaras")
	idwc_camaradest.InsertRow(0)
END IF

dw_2.SetTransObject(SqlCa)
dw_3.SetTransObject(SqlCa)
dw_5.SetTransObject(SqlCa)

dw_1.GetChild("cama_codigo", idwc_cam01)
idwc_cam01.SetTransObject(sqlca)
idwc_cam01.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_2.GetChild("cama_codigo", idwc_cam02)
idwc_cam02.SetTransObject(sqlca)
idwc_cam02.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

buscar	= "Lote:Scompute_3,Banda:Ncaex_nroban,Columna:Ncaex_nropos,Altura:Ncaex_nropis"
ordenar	= "Lote:compute_3,Banda:caex_nroban,Columna:caex_nropos,Altura:caex_nropis"

istr_Mant.Argumento[1]	=	Message.StringParm
istr_Mant.Argumento[2]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[6]	=	""

iuo_plantadesp			=	Create uo_plantadesp
iuo_camarasfrigoorig	=	Create uo_camarasfrigo
iuo_camarasfrigodest	=	Create uo_camarasfrigo
iuo_Lotes				=	Create uo_lotesfrutagranel
iuo_clientes				=	Create uo_cliente
iuo_tipomovtofruta		=	Create uo_tipomovtofruta

ii_cliente				=	gi_codexport
dw_5.SetTransObject(sqlca)
dw_1.SetTransObject(sqlca)

dw_5.InsertRow(0)
dw_5.SetItem(1, "plde_codigo", gstr_ParamPlanta.CodigoPlanta)
dw_5.SetItem(1, "mvce_fecmov",Today())
dw_5.SetItem(1, "espe_codigo", gstr_ParamPlanta.Codigoespecie)

dw_5.SetColumn("mvce_numero")
dw_5.SetFocus()

IF NOT iuo_tipomovtofruta.Existe(81,True,SqlCa) THEN
	pb_salir.PostEvent(Clicked!)
END IF

end event

event resize;call super::resize;Integer		li_posi_y, li_objeto

dw_1.Height	=	dw_1.Height / 2
dw_2.Height =	dw_1.Height
dw_2.Width	=	dw_1.Width
dw_2.x		=	dw_1.x
dw_2.y		=	dw_1.y + dw_1.Height + 2

dw_5.x		=	dw_1.x + ((dw_1.Width - dw_5.Width) / 2)

cb_aplica.y	= 	pb_imprimir.y + pb_imprimir.height + 10
cb_aplica.x	= 	pb_imprimir.x


end event

event ue_antesguardar;Long ll_numero, ll_fila
Integer li_planta, li_tipmov, li_cliente

Actualizamovto()

li_planta 	= 	dw_5.Object.plde_codigo[1]
li_tipmov 	= 	81
li_cliente	= 	dw_5.Object.clie_codigo[1]

IF dw_5.GetItemStatus(1,0,Primary!) = NewModified! THEN
	
   UPDATE	dbo.spro_movtocamarafgenca
		SET	mvce_numero = 0
		WHERE	1 = 2;
	
	SELECT	IsNull(Max(mvce_numero), 0) + 1
		INTO	:ll_Numero
		FROM	dbo.spro_movtocamarafgenca
  	  WHERE	plde_codigo	=	:li_Planta
		AND	tpmv_codigo	=	:li_tipmov
		AND 	clie_codigo = 	:li_cliente;
		
   	dw_5.Object.plde_codigo[1] = 	li_planta
	dw_5.Object.tpmv_codigo[1] = 	li_tipmov
	dw_5.Object.mvce_numero[1] = 	ll_numero
	dw_5.Object.clie_codigo[1]	=	li_cliente
ELSE
	
	ll_numero = dw_5.Object.mvce_numero[1]
	
END IF 

FOR ll_fila=1 TO dw_3.RowCount()
	IF dw_3.GetItemStatus(ll_fila,0,Primary!) = NewModified! THEN
		dw_3.Object.plde_codigo[ll_fila] = 	li_planta
   		dw_3.Object.tpmv_codigo[ll_fila] = 	li_tipmov
	  	dw_3.Object.mvce_numero[ll_fila] = 	ll_numero
	END IF	
	dw_3.Object.clie_codigo[ll_fila]	=	li_cliente
NEXT 	
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_spro_camaraexistefg
boolean visible = false
integer x = 105
integer y = 24
integer width = 2953
integer height = 388
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_spro_camaraexistefg
integer x = 3214
integer y = 360
integer taborder = 90
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(True)

dw_2.Reset()
dw_1.Reset()
dw_3.Reset()
dw_5.Reset()

dw_1.visible=TRUE
dw_2.visible=TRUE
dw_3.visible=False

pb_lectura.Enabled   = TRUE

dw_5.InsertRow(0)
dw_5.SetItem(1, "plde_codigo", gstr_ParamPlanta.CodigoPlanta)
dw_5.SetItem(1, "mvce_fecmov",Today())
dw_5.SetItem(1, "espe_codigo", gstr_ParamPlanta.Codigoespecie)

dw_5.Object.clie_codigo[1]				=	gi_codexport

istr_mant.argumento[2] = String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[3] = ""
istr_mant.argumento[4] = ""
istr_mant.argumento[6] = ""

dw_5.SetColumn("mvce_numero")
dw_5.SetFocus()



end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_spro_camaraexistefg
integer x = 3214
integer y = 124
integer taborder = 50
end type

event pb_lectura::clicked;Integer li_especie
Boolean lb_seguir=TRUE

li_especie = dw_5.Object.espe_codigo[1]

IF isnull(li_especie) THEN
	messagebox("Atención","Debe Seleccionar una Especie.")
	lb_seguir=FALSE
END IF	

IF istr_mant.argumento[3]="" THEN
	messagebox("Atención","Debe Seleccionar una Camara Origen.")
	lb_seguir=FALSE
END IF	

IF istr_mant.argumento[4]="" THEN
	messagebox("Atención","Debe Seleccionar una Camara Destino.")
	lb_seguir=FALSE
END IF	

IF lb_seguir THEN
	parent.triggerevent("ue_recuperadatos")
END IF
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_spro_camaraexistefg
boolean visible = false
integer x = 3031
integer y = 1584
integer taborder = 80
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_spro_camaraexistefg
boolean visible = false
integer x = 3031
integer y = 1348
integer taborder = 0
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_spro_camaraexistefg
integer x = 3214
integer y = 1508
integer taborder = 130
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_spro_camaraexistefg
integer x = 3214
integer y = 848
integer taborder = 120
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_spro_camaraexistefg
integer x = 3214
integer y = 684
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_spro_camaraexistefg
integer x = 206
integer y = 500
integer width = 2784
integer height = 680
integer taborder = 0
boolean titlebar = true
string title = "Camara Destino"
string dataobject = "dw_mant_spro_lotes_clasificados_origen"
boolean hscrollbar = true
boolean livescroll = false
end type

type dw_2 from datawindow within w_mant_mues_spro_camaraexistefg
integer x = 206
integer y = 1180
integer width = 2784
integer height = 656
integer taborder = 60
boolean bringtotop = true
boolean titlebar = true
string title = "Camara Origen"
string dataobject = "dw_mant_spro_lotes_clasificados_origen"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event clicked;//IF row > 0 THEN
//	IF IsSelected(row) THEN
//		SelectRow(row,False)
//	ELSE
//		SelectRow(row,True)
//	END IF
//END IF
String ls_tecla

IF Keydown(KeyShifT!) THEN
	ls_tecla = "Shift"
ELSEIF KeyDown(KeyControl!) THEN
	ls_tecla = "Control"
END IF

F_Selecciona(This,ls_tecla,row)

IF dw_2.GetSelectedRow(0) = 0 THEN
	pb_grabar.Enabled	=	False
ELSE
	pb_grabar.Enabled	=	True
END IF	
end event

event itemerror;RETURN 1
end event

event itemchanged;String ls_Columna,ls_Null

SetNull(ls_Null)
CHOOSE CASE dwo.name
		
	CASE "tot_bultos"
		IF Integer(Data) > 0 THEN			
			IF  dw_2.Object.caex_canbul[row] < Integer(Data)THEN
				MessageBox("Error", "Bultos A traspasar es Mayor a Bultos Existentes.",Exclamation! )			
				dw_2.SetItem(row,"tot_bultos",0)	
				RETURN 1
			END IF			
		ELSE
			MessageBox("Error", "Bultos A traspasar debe ser Mayor a Cero.",Exclamation! )			
			dw_2.SetItem(row,"tot_bultos",0)	
			RETURN 1
		END IF	
			
	END CHOOSE		
		
end event

event doubleclicked;IF row > 0 THEN
	dw_2.Object.tot_bultos[row] = dw_2.Object.caex_canbul[row]
END IF
end event

type cb_aplica from commandbutton within w_mant_mues_spro_camaraexistefg
integer x = 3214
integer y = 1116
integer width = 302
integer height = 100
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Aplica"
end type

event clicked;Integer	li_filas

li_filas	= 	0
li_filas = 	dw_2.GetSelectedRow(li_filas)

//DO WHILE li_filas > 0
FOR li_filas = 1 TO dw_2.RowCount()
	IF dw_2.IsSelected(li_filas) THEN
		dw_2.RowsMove(li_filas, li_filas, Primary!, dw_1, dw_1.RowCount() + 1, Primary!)
		li_filas = li_filas - 1
	END IF
NEXT
//LOOP

FOR li_filas = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(li_filas, 0, Primary!) = NewModified!THEN
		
		dw_1.Object.cama_codigo[li_filas]	=	Integer(istr_Mant.Argumento[4])
		dw_1.SetItemStatus(li_filas, 0, Primary!, DataModified!)
	END IF
NEXT
end event

type dw_4 from datawindow within w_mant_mues_spro_camaraexistefg
boolean visible = false
integer x = 3319
integer y = 1248
integer width = 197
integer height = 140
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_mues_spro_movtocamarafgenca"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_mant_mues_spro_camaraexistefg
integer x = 110
integer y = 32
integer width = 2907
integer height = 344
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_mues_movtocamaraenca"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Null
String	ls_columna

ls_columna = GetColumnName()
SetNull(ls_Null)

CHOOSE CASE ls_columna

	CASE "mvce_numero"
		 istr_mant.Argumento[6] = ""
		 IF existefolio(Long(data)) THEN
			istr_mant.argumento[6] = data
			parent.triggerevent("ue_recuperadatos")
		 ELSE
			This.SetItem(1, "mvce_numero", Long(ls_Null))
			RETURN 1
		 END IF	
		
	CASE "plde_codigo"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "plde_codigo", integer(ls_Null))
			This.SetFocus()
			RETURN 1
		ELSE
			ii_Planta	=	Integer(data)
		END IF

	CASE "cama_codori"
		IF NOT iuo_CamarasFrigoorig.Existe(gstr_paramplanta.codigoplanta,Integer(data),True,SqlCa) THEN
			This.SetItem(1, "cama_codori", integer(ls_Null))
			This.SetFocus()
			RETURN 1
		ELSE
			istr_Mant.Argumento[3]	=	Data
		END IF

	CASE "cama_coddes"
		IF NOT iuo_CamarasFrigodest.Existe(gstr_paramplanta.codigoplanta,Integer(data),True,SqlCa) THEN
			This.SetItem(1, "cama_coddes", integer(ls_Null))
			This.SetFocus()
			RETURN 1
		ELSE
			istr_Mant.Argumento[4]	=	Data
		END IF
		
	CASE "mvce_fecmov"
		istr_Mant.Argumento[5] = Data
		
	CASE "clie_codigo"
		IF iuo_clientes.Existe(Integer(data), True, sqlca) THEN
			ii_cliente = Integer(data)
		ELSE
			This.SetItem(1, "clie_codigo", integer(ls_Null))
			This.SetFocus()
			RETURN 1
		END IF
		
END CHOOSE

end event

event itemerror;Return 1
end event

event buttonclicked;
CHOOSE CASE dwo.Name
	
	CASE "b_folio"
		buscafolio()		


END CHOOSE
end event

type dw_3 from datawindow within w_mant_mues_spro_camaraexistefg
boolean visible = false
integer x = 151
integer y = 496
integer width = 2857
integer height = 872
integer taborder = 110
string title = "Movimiento de Camara"
string dataobject = "dw_mant_mues_movtocamarafg"
end type

