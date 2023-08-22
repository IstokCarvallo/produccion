$PBExportHeader$w_info_seleccion_fruta_a_proceso.srw
forward
global type w_info_seleccion_fruta_a_proceso from w_mant_encab_deta
end type
type tab_1 from tab within w_info_seleccion_fruta_a_proceso
end type
type tp_1 from userobject within tab_1
end type
type dw_madurez from datawindow within tp_1
end type
type tp_1 from userobject within tab_1
dw_madurez dw_madurez
end type
type tp_2 from userobject within tab_1
end type
type dw_colorfondo from datawindow within tp_2
end type
type tp_2 from userobject within tab_1
dw_colorfondo dw_colorfondo
end type
type tp_3 from userobject within tab_1
end type
type dw_cubrim from datawindow within tp_3
end type
type tp_3 from userobject within tab_1
dw_cubrim dw_cubrim
end type
type tp_4 from userobject within tab_1
end type
type dw_calibre from datawindow within tp_4
end type
type tp_4 from userobject within tab_1
dw_calibre dw_calibre
end type
type tab_1 from tab within w_info_seleccion_fruta_a_proceso
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
tp_4 tp_4
end type
end forward

global type w_info_seleccion_fruta_a_proceso from w_mant_encab_deta
integer width = 3589
integer height = 2052
string title = "CONTROL DE CALIDAD RECEPCION"
string menuname = ""
boolean resizable = false
event ue_imprimir ( )
tab_1 tab_1
end type
global w_info_seleccion_fruta_a_proceso w_info_seleccion_fruta_a_proceso

type variables
uo_plantadesp        iuo_planta
uo_especie  			iuo_especie
uo_productores			iuo_productores
uo_camarasfrigo		iuo_camarasfrigo
uo_grupoespecie		iuo_grupoespecie
uo_subgrupoespecie	iuo_subgrupoespecie
uo_variedades			iuo_variedades
uo_tratamientofrio	iuo_tratamientofrio
uo_periodofrio			iuo_periodofrio
uo_parammadurez      iuo_madurez

Boolean							ib_modifica
DataWindowChild   			dw_especies, idwc_planta, idwc_especie, idwc_variedad, &
									idwc_tipofrio, idwc_periodofrio, idwc_categoria, &
									idwc_color_fondo, idwc_camara, idwc_grupo, idwc_subgrupo, &
									idwc_exporta, idwc_productor
									
DataWindow						dw_3, dw_4, dw_5, dw_6, dw_7, dw_8, dw_9
str_variedad					istr_variedad
str_categoria					istr_categoria

String							is_columna

DataStore			ids_Base
end variables

forward prototypes
public subroutine habilitacolumnas (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine habilitadetalle (boolean habilita)
public subroutine imprime_seleccion ()
public function boolean existeinforme (string as_columna, string as_valor)
public subroutine habilitacolor ()
public function boolean eliminadatos ()
public subroutine generadetalle (boolean nuevo)
public subroutine habilitaparametros (integer ai_especie, integer ai_variedad, integer ai_grupo, integer ai_subgrupo)
end prototypes

event ue_imprimir();SetPointer(HourGlass!)

Long		ll_fila
Integer  li_resumen

str_info	lstr_info

lstr_info.titulo	= "SELECCION DE FRUTA A PROCESO"
lstr_info.copias	= 1
li_resumen = dw_2.Object.sepr_resren[1]

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_seleccion_fruta_proceso_com"

vinf.dw_1.SetTransObject(sqlca)

ll_fila = vinf.dw_1.Retrieve(integer(istr_mant.argumento[2]),istr_mant.argumento[1],li_resumen)

IF ll_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_fila =0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')
	vinf.Visible	= True
	vinf.Enabled	= True
	
   
END IF



end event

public subroutine habilitacolumnas (boolean habilita);IF Habilita THEN
	dw_2.object.lote_espcod.Protect  			=  1
	dw_2.Object.lote_espcod.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.vari_codigo.Protect				=	0
	dw_2.Object.vari_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.frio_tipofr.Protect				=	0
	dw_2.Object.frio_tipofr.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.pefr_codigo.Protect				=	0
	dw_2.Object.pefr_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.cocc_codigo.Protect				=	0
	dw_2.Object.cocc_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.cate_codigo.Protect				=	0
	dw_2.Object.cate_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.fgcc_nrofol.Protect				=	0
	dw_2.Object.fgcc_nrofol.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.fgcc_feccon.Protect				=	0
	dw_2.Object.fgcc_feccon.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.fgcc_horcon.Protect				=	0
	dw_2.Object.fgcc_horcon.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.fgcc_llenop.Protect				=	0
	dw_2.Object.fgcc_llenop.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.fgcc_encarp.Protect				=	0
	dw_2.Object.fgcc_encarp.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.fgcc_empper.Protect				=	0
	dw_2.Object.fgcc_empper.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.fgcc_observ.Protect				=	0
	dw_2.Object.fgcc_observ.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.cmen_califi.Protect				=	0
	dw_2.Object.cmen_califi.BackGround.Color	=	RGB(255,255,255)
ELSE
	dw_2.object.lote_espcod.Protect  			=  0
	dw_2.Object.lote_espcod.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.vari_codigo.Protect				=	1
	dw_2.Object.vari_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.frio_tipofr.Protect				=	1
	dw_2.Object.frio_tipofr.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.pefr_codigo.Protect				=	1
	dw_2.Object.pefr_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.cocc_codigo.Protect				=	1
	dw_2.Object.cocc_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.cate_codigo.Protect				=	1
	dw_2.Object.cate_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.fgcc_nrofol.Protect				=	1
	dw_2.Object.fgcc_nrofol.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.fgcc_feccon.Protect				=	1
	dw_2.Object.fgcc_feccon.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.fgcc_horcon.Protect				=	1
	dw_2.Object.fgcc_horcon.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.fgcc_llenop.Protect				=	1
	dw_2.Object.fgcc_llenop.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.fgcc_encarp.Protect				=	1
	dw_2.Object.fgcc_encarp.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.fgcc_empper.Protect				=	1
	dw_2.Object.fgcc_empper.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.fgcc_observ.Protect				=	1
	dw_2.Object.fgcc_observ.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.cmen_califi.Protect				=	1
	dw_2.Object.cmen_califi.BackGround.Color	=	RGB(192,192,192)
END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno=FALSE

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_3.Update(True, False) = 1 THEN
		IF dw_4.Update(True, False) = 1 THEN
			IF dw_5.Update(True, False) = 1 THEN
				IF dw_6.Update(True, False) = 1 THEN
					IF dw_7.Update(True, False) = 1 THEN
						IF dw_9.Update(True, False) = 1 THEN
							IF dw_2.Update(True, False) = 1 THEN
								Commit;
					
								IF sqlca.SQLCode <> 0 THEN
									F_ErrorBaseDatos(sqlca, This.Title)
									Rollback;
								ELSE
									lb_Retorno	=	True
					
									dw_2.ResetUpdate()
									dw_3.ResetUpdate()
									dw_4.ResetUpdate()
									dw_5.ResetUpdate()
									dw_6.ResetUpdate()
									dw_7.ResetUpdate()
									dw_9.ResetUpdate()
								END IF
							ELSE
								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;
							END IF	
						ELSE
							F_ErrorBaseDatos(sqlca, This.Title)
							RollBack;
						END IF	
					ELSE
						F_ErrorBaseDatos(sqlca, This.Title)
						RollBack;
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_3.Update(True, False) = 1 THEN
			IF dw_4.Update(True, False) = 1 THEN
				IF dw_5.Update(True, False) = 1 THEN
					IF dw_6.Update(True, False) = 1 THEN
											
						Commit;
						
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
							Rollback;
						ELSE
							lb_Retorno	=	True
				
							dw_2.ResetUpdate()
							dw_3.ResetUpdate()
							dw_4.ResetUpdate()
							dw_5.ResetUpdate()
							dw_6.ResetUpdate()
					
						END IF
					
					ELSE
						F_ErrorBaseDatos(sqlca, This.Title)
						RollBack;
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine habilitadetalle (boolean habilita);tab_1.tp_1.Enabled	=	Habilita
tab_1.tp_2.Enabled	=	Habilita
tab_1.tp_3.Enabled	=	Habilita
tab_1.tp_4.Enabled	=	Habilita

end subroutine

public subroutine imprime_seleccion ();SetPointer(HourGlass!)

w_informes				linf

Long		ll_fila

str_info	lstr_info

lstr_info.titulo	= "SELECCION DE FRUTA A PROCESO"
lstr_info.copias	= 1

OpenWithParm(linf,lstr_info)

linf.dw_1.DataObject = "dw_info_frutaproceso_seleccion_datos"

linf.dw_1.SetTransObject(sqlca)

ll_fila = linf.dw_1.Retrieve(istr_mant.argumento[1],integer(istr_mant.argumento[2]))

IF ll_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_fila =0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(linf.dw_1)
	linf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	linf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')
	linf.Visible	= True
	linf.Enabled	= True
END IF

SetPointer(Arrow!)

end subroutine

public function boolean existeinforme (string as_columna, string as_valor);Integer li_exporta, li_especie, li_cuenta
String  ls_usuario

ls_Usuario = dw_2.Object.sepr_usumod[1]
li_especie = dw_2.Object.espe_codigo[1]

CHOOSE CASE as_columna
		
	CASE "sepr_usumod"
		ls_Usuario = as_valor
		
	CASE "espe_codigo"
		li_especie = integer(as_valor)
	
END CHOOSE

SELECT count(espe_codigo) INTO :li_cuenta
  FROM dba.spro_selecfrutaproc
 WHERE sepr_usumod = :ls_usuario
   AND espe_codigo = :li_especie;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Selección Fruta Proceso.")
	
	RETURN FALSE
ELSE
	
	IF isnull(li_cuenta) THEN li_cuenta = 0
	
	IF li_cuenta = 0 THEN
	   RETURN FALSE
	ELSE
		RETURN TRUE
	END IF	
END IF	

RETURN TRUE
end function

public subroutine habilitacolor ();
IF dw_2.RowCount() > 0 THEN
	
	IF NOT isnull(dw_2.Object.grva_codsub[1]) THEN
		dw_2.Object.grva_codsub.BackGround.Color = RGB(255,255,255)
	END IF	
	
	IF dw_2.Object.sepr_plttod[1] = 0 THEN
		dw_2.Object.plde_codigo.Protect = 0
		dw_2.Object.plde_codigo.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_2.Object.sepr_camtod[1] = 0 THEN
		dw_2.Object.cama_codigo.Protect = 0
		dw_2.Object.cama_codigo.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_2.Object.sepr_fritod[1] = 0 THEN
		dw_2.Object.frio_tipofr.Protect = 0
		dw_2.Object.frio_tipofr.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_2.Object.sepr_pertod[1] = 0 THEN
		dw_2.Object.pefr_codigo.Protect = 0
		dw_2.Object.pefr_codigo.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_2.Object.sepr_bultod[1] = 0 THEN
		dw_2.Object.sepr_bulmin.Protect = 0
		dw_2.Object.sepr_bulmin.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_2.Object.sepr_embtod[1] = 0 THEN
		dw_2.Object.sepr_pormin.Protect = 0
		dw_2.Object.sepr_pormin.BackGround.Color = RGB(255,255,255)
	END IF

	IF dw_2.Object.sepr_nivtod[1] = 0 THEN
		dw_2.Object.sepr_nivcal.Protect = 0
		dw_2.Object.sepr_nivcal.BackGround.Color = RGB(255,255,255)
	END IF
END IF

IF dw_3.RowCount() > 0 THEN

	IF dw_3.Object.rama_ssoltd[1] = 0 THEN
		dw_3.Object.rama_ssolin.Protect = 0
		dw_3.Object.rama_ssolin.BackGround.Color = RGB(255,255,255)
		dw_3.Object.rama_ssolfi.Protect = 0
		dw_3.Object.rama_ssolfi.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_3.Object.rama_preetd[1] = 0 THEN
		dw_3.Object.rama_preein.Protect = 0
		dw_3.Object.rama_preein.BackGround.Color = RGB(255,255,255)
		dw_3.Object.rama_preefi.Protect = 0
		dw_3.Object.rama_preefi.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_3.Object.rama_prehtd[1] = 0 THEN
		dw_3.Object.rama_prehin.Protect = 0
		dw_3.Object.rama_prehin.BackGround.Color = RGB(255,255,255)
		dw_3.Object.rama_prehfi.Protect = 0
		dw_3.Object.rama_prehfi.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_3.Object.rama_preatd[1] = 0 THEN
		dw_3.Object.rama_preain.Protect = 0
		dw_3.Object.rama_preain.BackGround.Color = RGB(255,255,255)
		dw_3.Object.rama_preafi.Protect = 0
		dw_3.Object.rama_preafi.BackGround.Color = RGB(255,255,255)
	END IF
	
	
	IF dw_3.Object.rama_talmtd[1] = 0 THEN
		dw_3.Object.rama_talmin.Protect = 0
		dw_3.Object.rama_talmin.BackGround.Color = RGB(255,255,255)
		dw_3.Object.rama_talmfi.Protect = 0
		dw_3.Object.rama_talmfi.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_3.Object.rama_acidtd[1] = 0 THEN
		dw_3.Object.rama_acidin.Protect = 0
		dw_3.Object.rama_acidin.BackGround.Color = RGB(255,255,255)
		dw_3.Object.rama_acidfi.Protect = 0
		dw_3.Object.rama_acidfi.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_3.Object.rama_sofitd[1] = 0 THEN
		dw_3.Object.rama_sofiin.Protect = 0
		dw_3.Object.rama_sofiin.BackGround.Color = RGB(255,255,255)
		dw_3.Object.rama_sofifi.Protect = 0
		dw_3.Object.rama_sofifi.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_3.Object.rama_matstd[1] = 0 THEN
		dw_3.Object.rama_matsin.Protect = 0
		dw_3.Object.rama_matsin.BackGround.Color = RGB(255,255,255)
		dw_3.Object.rama_matsfi.Protect = 0
		dw_3.Object.rama_matsfi.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_3.Object.rama_nsemtd[1] = 0 THEN
		dw_3.Object.rama_nsemin.Protect = 0
		dw_3.Object.rama_nsemin.BackGround.Color = RGB(255,255,255)
		dw_3.Object.rama_nsemfi.Protect = 0
		dw_3.Object.rama_nsemfi.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_3.Object.rama_coactd[1] = 0 THEN
		dw_3.Object.rama_coacin.Protect = 0
		dw_3.Object.rama_coacin.BackGround.Color = RGB(255,255,255)
		dw_3.Object.rama_coacfi.Protect = 0
		dw_3.Object.rama_coacfi.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_3.Object.rama_comotd[1] = 0 THEN
		dw_3.Object.rama_comoin.Protect = 0
		dw_3.Object.rama_comoin.BackGround.Color = RGB(255,255,255)
		dw_3.Object.rama_comofi.Protect = 0
		dw_3.Object.rama_comofi.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_3.Object.rama_haritd[1] = 0 THEN
		dw_3.Object.rama_hariin.Protect = 0
		dw_3.Object.rama_hariin.BackGround.Color = RGB(255,255,255)
		dw_3.Object.rama_harifi.Protect = 0
		dw_3.Object.rama_harifi.BackGround.Color = RGB(255,255,255)
	END IF
	
	IF dw_3.Object.rama_grastd[1] = 0 THEN
		dw_3.Object.rama_grasin.Protect = 0
		dw_3.Object.rama_grasin.BackGround.Color = RGB(255,255,255)
		dw_3.Object.rama_grasfi.Protect = 0
		dw_3.Object.rama_grasfi.BackGround.Color = RGB(255,255,255)
	END IF
	
END IF	

end subroutine

public function boolean eliminadatos ();String ls_Usuario
Integer li_especie

ls_Usuario = dw_2.Object.sepr_usumod[1]
li_especie = dw_2.Object.espe_codigo[1]

DELETE dba.spro_rangoscolorfondo
 WHERE sepr_usumod = :ls_Usuario
   AND espe_codigo = :li_especie;

  IF sqlca.SQLCode = -1 THEN
	  F_ErrorBaseDatos(sqlca, "Lectura de Tabla Rango de Color de Fondo.")
	  ROLLBACK;
	  RETURN FALSE
  END IF
  
DELETE dba.spro_rangoscolorcubri
 WHERE sepr_usumod = :ls_Usuario
   AND espe_codigo = :li_especie;

  IF sqlca.SQLCode = -1 THEN
	  F_ErrorBaseDatos(sqlca, "Lectura de Tabla Rangos de Color de Cubrimiento.")
	  ROLLBACK;
	  RETURN FALSE
  END IF
  
DELETE dba.spro_rangosdistricalib
 WHERE sepr_usumod = :ls_Usuario
   AND espe_codigo = :li_especie;

  IF sqlca.SQLCode = -1 THEN
	  F_ErrorBaseDatos(sqlca, "Lectura de Tabla Rangos de Distribución de Calibres.")
	  ROLLBACK;
	  RETURN FALSE
  END IF

  Commit;
IF sqlca.SQLCode <> 0 THEN
	F_ErrorBaseDatos(sqlca, This.Title)
	Rollback;
	RETURN FALSE
END IF

RETURN TRUE	
end function

public subroutine generadetalle (boolean nuevo);Long		ll_Fila, ll_Filas, ll_FilaNueva
Integer	li_Null, li_Contador, li_grupo, li_subgrupo, li_variedad
String   ls_Mensaje

SetNull(li_Null)

istr_Mant.Argumento[2] = String(dw_2.Object.espe_codigo[1])
istr_Mant.Argumento[3] = String(dw_2.Object.vari_codigo[1])

li_grupo 	= dw_2.Object.grva_codigo[1]
li_subgrupo = dw_2.Object.grva_codsub[1]
li_variedad = dw_2.Object.vari_codigo[1]


dw_4.GetChild("cofo_codigo", idwc_color_fondo)
idwc_color_fondo.SetTransObject(SqlCa)


IF idwc_color_fondo.Retrieve(Integer(istr_mant.argumento[2]),li_grupo,li_subgrupo, &
									  li_variedad) = 0 THEN
	IF idwc_color_fondo.Retrieve(Integer(istr_mant.argumento[2]),li_grupo,li_subgrupo, &
										  li_Null) = 0 THEN	
		IF idwc_color_fondo.Retrieve(Integer(istr_mant.argumento[2]),li_grupo,li_Null, &
											  li_Null) = 0 THEN	
			IF idwc_color_fondo.Retrieve(Integer(istr_mant.argumento[2]),li_Null,li_Null, &
												  li_Null) = 0 THEN	
				idwc_color_fondo.InsertRow(0)
			END IF	
		END IF	
	END IF
END IF

IF Nuevo THEN
	dw_3.SetItem(1,'rama_coacle',0)
	dw_3.SetItem(1,'rama_coacmo',0)
	dw_3.SetItem(1,'rama_coacse',0)
	dw_3.SetItem(1,'rama_comole',0)
	dw_3.SetItem(1,'rama_comomo',0)
	dw_3.SetItem(1,'rama_comose',0)
	dw_3.SetItem(1,'rama_harile',0)
	dw_3.SetItem(1,'rama_harimo',0)
	dw_3.SetItem(1,'rama_harise',0)
	dw_3.SetItem(1,'rama_grasle',0)
	dw_3.SetItem(1,'rama_grasmo',0)
	dw_3.SetItem(1,'rama_grasse',0)
END IF


//	Llena Color de Fondo
IF dw_4.RowCount() >= 0 THEN
	ids_Base.DataObject	=	"dw_sele_spro_colordefondo"
	ids_Base.SetTransObject(sqlca)
	
	ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]), li_grupo, li_subgrupo, &
											li_variedad)
	
	IF ll_Filas = 0 THEN
	
		ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]), li_grupo, li_subgrupo, &
												li_Null)
	END IF											
		
	IF ll_Filas = 0 THEN
		
		ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]), li_grupo, li_Null, &
												li_Null)
	END IF												
		
	IF ll_Filas = 0 THEN
		
		ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]), li_Null, li_Null, &
												li_Null)
	END IF
	
	IF ll_Filas = 0 THEN
      li_Contador ++
		ls_Mensaje      =ls_mensaje + ("~rColor de Fondo ")
	ELSE
		FOR ll_Filas = 1 TO ids_Base.RowCount()
			ll_FilaNueva = 0
			ll_FilaNueva = dw_4.Find("cofo_codigo = " + String(ids_Base.Object.cofo_codigo[ll_Filas]), &
			                          1, dw_4.RowCount()) 
											  
			IF ll_FilaNueva <=0 THEN								  

				ll_Fila	=	dw_4.InsertRow(0)
						
				dw_4.Object.cofo_codigo[ll_Fila]	=	ids_Base.Object.cofo_codigo[ll_Filas]
				dw_4.Object.raco_todos[ll_Fila]	=	1
				dw_4.Object.raco_codano[ll_Fila]	=	ids_Base.Object.cofo_codano[ll_filas]
			END IF	
		NEXT
	END IF
END IF

//	Llena Color de Cubrimiento
IF dw_5.RowCount() >= 0 THEN
	ids_Base.DataObject	=	"dw_mues_spro_especiecatego"
	ids_Base.SetTransObject(sqlca)
	
	ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]),&
											Integer(istr_Mant.Argumento[3]))
	
	IF ll_Filas = 0 THEN
		ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]),&
												li_Null)
	END IF

	IF ll_Filas = 0 THEN
		li_Contador ++
		ls_Mensaje      =ls_mensaje + ("~rColores de Cubrimiento ")
	ELSE
		FOR ll_Filas = 1 TO ids_Base.RowCount()
			ll_FilaNueva = 0 
			ll_FilaNueva = dw_5.Find("cate_codigo = " + String(ids_Base.Object.cate_codigo[ll_Filas]), &
			                          1, dw_5.RowCount())
			
			IF ll_FilaNueva <= 0 THEN

				ll_Fila	=	dw_5.InsertRow(0)
			
				dw_5.Object.cate_codigo[ll_Fila]	=	ids_Base.Object.cate_codigo[ll_Filas]
				dw_5.Object.racu_todos[ll_Fila]	=	1			
				dw_5.Object.racu_codano[ll_Fila]	=	ids_Base.Object.esca_codano[ll_Filas]
			END IF	
		NEXT
	END IF
END IF


//	Llena Distribución de Calibre
IF dw_6.RowCount() >= 0 THEN
	
	ids_Base.DataObject	=	"dw_mues_spro_espevarigrucal"
	ids_Base.SetTransObject(sqlca)
	
	ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]),&
											Integer(istr_Mant.Argumento[3]))
	
	IF ll_Filas = 0 THEN
		ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]),&
												li_Null)
	END IF

	FOR ll_Filas = 1 TO ids_Base.RowCount()
		ll_FilaNueva = 0 
		ll_FilaNueva = dw_6.Find("evdc_grucal = '" + ids_Base.Object.evdc_grucal[ll_Filas] + "'", &
		                          1, dw_6.RowCount())
		IF ll_FilaNueva <= 0 THEN								  

			ll_Fila	=	dw_6.InsertRow(0)
			
			dw_6.Object.evdc_grucal[ll_Fila]	=	ids_Base.Object.evdc_grucal[ll_Filas]
			dw_6.Object.raca_todos[ll_Fila]	=	1
			dw_6.Object.raca_codano[ll_Fila]	=	ids_Base.Object.evdc_codano[ll_Filas]
		END IF		
	NEXT

END IF	

end subroutine

public subroutine habilitaparametros (integer ai_especie, integer ai_variedad, integer ai_grupo, integer ai_subgrupo);Integer li_solsol, li_preecu, li_prehom, li_preapi, li_tesalm, li_coracu, li_cormoh, &
       li_acidez, li_harino, li_grasit, li_solfin, li_matsec, li_nrosem, li_Null
		 
SetNull(li_Null)		 

IF iuo_variedades.existe(ai_especie,ai_variedad, FALSE,SQLCA) THEN
	ai_grupo 	= 	iuo_variedades.grupo
	ai_subgrupo	=	iuo_variedades.subgrupo
END IF

IF NOT iuo_Madurez.Existe(ai_especie,ai_grupo,ai_subgrupo, ai_variedad, &
											FALSE, Sqlca) THEN
	ai_variedad = -1										
	IF NOT iuo_Madurez.Existe(ai_especie,ai_grupo,ai_subgrupo, -1, &
											FALSE, Sqlca) THEN
		ai_subgrupo = -1								
		IF NOT iuo_Madurez.Existe(ai_especie,ai_grupo,-1, -1, FALSE, Sqlca) THEN
			ai_grupo = -1								
			IF NOT iuo_Madurez.Existe(ai_especie,-1,-1, -1, FALSE, Sqlca) THEN
			END IF
		END IF
	END IF
END IF	


SELECT MAX(scm.copam_solsol), MAX(scm.copam_preecu), MAX(scm.copam_prehom),
       MAX(scm.copam_preapi), MAX(scm.copam_tesalm), MAX(scm.copam_coracu),
		 MAX(scm.copam_cormoh), MAX(scm.copam_acidez), MAX(scm.copam_harino),
		 MAX(scm.copam_grasit), MAX(scm.copam_solfin), MAX(scm.copam_matsec),
		 MAX(scm.copam_nrosem)
  INTO :li_solsol, :li_preecu, :li_prehom, :li_preapi, :li_tesalm, :li_coracu,
       :li_cormoh, :li_acidez, :li_harino, :li_grasit, :li_solfin, :li_matsec,
		 :li_nrosem
  FROM dba.spro_contparamadurez scm
 WHERE scm.espe_codigo = :ai_especie
	AND isnull(scm.grva_codigo,-1) = :ai_grupo
	AND isnull(scm.grva_codsub,-1) = :ai_subgrupo
	AND isnull(scm.vari_codigo,-1) = :ai_variedad;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Parametros de Madurez")
ELSE
	
	IF li_solsol=0 OR isnull(li_solsol) THEN
		dw_3.Object.rama_ssolin.Protect = 1
		dw_3.Object.rama_ssolfi.Protect = 1
		dw_3.Object.rama_ssoltd.Protect = 1
      dw_3.Setitem(1,"rama_ssolin",li_Null)
		dw_3.Setitem(1,"rama_ssolfi",li_Null)
		dw_3.Setitem(1,"rama_ssoltd",1)
		dw_3.Object.rama_ssoltd.Color = RGB(140,140,140)
	ELSE
		dw_3.Object.rama_ssoltd.Color = RGB(0,0,0)
		dw_3.Object.rama_ssoltd.Protect = 0
	END IF	
	
	IF li_preecu = 0 OR isnull(li_preecu) THEN
		dw_3.Object.rama_preein.Protect = 1
		dw_3.Object.rama_preefi.Protect = 1
		dw_3.Object.rama_preetd.Protect = 1
      dw_3.Setitem(1,"rama_preein",li_Null)
		dw_3.Setitem(1,"rama_preefi",li_Null)
		dw_3.Setitem(1,"rama_preetd",1)
		dw_3.Object.rama_preetd.Color = RGB(140,140,140)
	ELSE
		dw_3.Object.rama_preetd.Color = RGB(0,0,0)
		dw_3.Object.rama_preetd.Protect = 0
	END IF	
	
	IF li_prehom = 0 OR isnull(li_prehom) THEN
		dw_3.Object.rama_prehin.Protect = 1
		dw_3.Object.rama_prehfi.Protect = 1
		dw_3.Object.rama_prehtd.Protect = 1
      dw_3.Setitem(1,"rama_prehin",li_Null)
		dw_3.Setitem(1,"rama_prehfi",li_Null)
		dw_3.Setitem(1,"rama_prehtd",1)
		dw_3.Object.rama_prehtd.Color = RGB(140,140,140)
	ELSE
		dw_3.Object.rama_prehtd.Color = RGB(0,0,0)
		dw_3.Object.rama_prehtd.Protect = 0
	END IF	

	IF li_preapi=0 OR isnull(li_preapi) THEN
		dw_3.Object.rama_preain.Protect = 1
		dw_3.Object.rama_preafi.Protect = 1
		dw_3.Object.rama_preatd.Protect = 1
      dw_3.Setitem(1,"rama_preain",li_Null)
		dw_3.Setitem(1,"rama_preafi",li_Null)
		dw_3.Setitem(1,"rama_preatd",1)
		dw_3.Object.rama_preatd.Color = RGB(140,140,140)
	ELSE
		dw_3.Object.rama_preatd.Color = RGB(0,0,0)
		dw_3.Object.rama_preatd.Protect = 0
	END IF	
		
	IF li_tesalm=0 OR isnull(li_tesalm) THEN
		dw_3.Object.rama_talmin.Protect = 1
		dw_3.Object.rama_talmfi.Protect = 1
		dw_3.Object.rama_talmtd.Protect = 1
      dw_3.Setitem(1,"rama_talmin",li_Null)
		dw_3.Setitem(1,"rama_talmfi",li_Null)
		dw_3.Setitem(1,"rama_talmtd",1)
		dw_3.Object.rama_talmtd.Color = RGB(140,140,140)
	ELSE
		dw_3.Object.rama_talmtd.Color = RGB(0,0,0)
		dw_3.Object.rama_talmtd.Protect = 0
	END IF	
	
	IF li_coracu=0 OR isnull(li_coracu) THEN
		dw_3.Object.rama_coacin.Protect = 1
		dw_3.Object.rama_coacfi.Protect = 1
		dw_3.Object.rama_coacle.Protect = 1
		dw_3.Object.rama_coacmo.Protect = 1
		dw_3.Object.rama_coacse.Protect = 1
		dw_3.Object.rama_coactd.Protect = 1
		dw_3.Setitem(1,"rama_coacin",li_Null)
		dw_3.Setitem(1,"rama_coacfi",li_Null)
		dw_3.Setitem(1,"rama_coacle",0)
		dw_3.Setitem(1,"rama_coacmo",0)
		dw_3.Setitem(1,"rama_coacse",0)
		dw_3.Setitem(1,"rama_coactd",1)
		dw_3.Object.rama_coactd.Color = RGB(140,140,140)
	ELSE
		dw_3.Object.rama_coactd.Color = RGB(0,0,0)
		dw_3.Object.rama_coactd.Protect = 0
	END IF
	
	IF li_cormoh=0 OR isnull(li_cormoh) THEN
		dw_3.Object.rama_comoin.Protect = 1
		dw_3.Object.rama_comofi.Protect = 1
		dw_3.Object.rama_comole.Protect = 1
		dw_3.Object.rama_comomo.Protect = 1
		dw_3.Object.rama_comose.Protect = 1
		dw_3.Object.rama_comotd.Protect = 1
      dw_3.Setitem(1,"rama_comoin",li_Null)
		dw_3.Setitem(1,"rama_comofi",li_Null)
		dw_3.Setitem(1,"rama_comole",0)
		dw_3.Setitem(1,"rama_comomo",0)
		dw_3.Setitem(1,"rama_comose",0)
		dw_3.Setitem(1,"rama_comotd",1)
		dw_3.Object.rama_comotd.Color = RGB(140,140,140)
	ELSE
		dw_3.Object.rama_comotd.Color = RGB(0,0,0)
		dw_3.Object.rama_comotd.Protect = 0
	END IF

	IF li_acidez=0 OR isnull(li_acidez) THEN
		dw_3.Object.rama_acidin.Protect = 1
		dw_3.Object.rama_acidfi.Protect = 1
		dw_3.Object.rama_acidtd.Protect = 1
      dw_3.Setitem(1,"rama_acidin",li_Null)
		dw_3.Setitem(1,"rama_acidfi",li_Null)
		dw_3.Setitem(1,"rama_acidtd",1)
		dw_3.Object.rama_acidtd.Color = RGB(140,140,140)
	ELSE
		dw_3.Object.rama_acidtd.Color = RGB(0,0,0)
		dw_3.Object.rama_acidtd.Protect = 0
	END IF
	
	IF li_harino=0 OR isnull(li_harino) THEN
		dw_3.Object.rama_hariin.Protect = 1
		dw_3.Object.rama_harifi.Protect = 1
		dw_3.Object.rama_harile.Protect = 1
		dw_3.Object.rama_harimo.Protect = 1
		dw_3.Object.rama_harise.Protect = 1
		dw_3.Object.rama_haritd.Protect = 1
      dw_3.Setitem(1,"rama_hariin",li_Null)
		dw_3.Setitem(1,"rama_harifi",li_Null)
		dw_3.Setitem(1,"rama_harile",0)
		dw_3.Setitem(1,"rama_harimo",0)
		dw_3.Setitem(1,"rama_harise",0)
		dw_3.Setitem(1,"rama_haritd",1)
		dw_3.Object.rama_haritd.Color = RGB(140,140,140)
	ELSE
		dw_3.Object.rama_haritd.Color = RGB(0,0,0)
		dw_3.Object.rama_haritd.Protect = 0
	END IF
	
	IF li_grasit=0 OR isnull(li_grasit) THEN
		dw_3.Object.rama_grasin.Protect = 1
		dw_3.Object.rama_grasfi.Protect = 1
		dw_3.Object.rama_grasle.Protect = 1
		dw_3.Object.rama_grasmo.Protect = 1
		dw_3.Object.rama_grasse.Protect = 1
		dw_3.Object.rama_grastd.Protect = 1
      dw_3.Setitem(1,"rama_grasin",li_Null)
		dw_3.Setitem(1,"rama_grasfi",li_Null)
		dw_3.Setitem(1,"rama_grasle",0)		
		dw_3.Setitem(1,"rama_grasmo",0)
		dw_3.Setitem(1,"rama_grasse",0)
		dw_3.Setitem(1,"rama_grastd",1)
		dw_3.Object.rama_grastd.Color = RGB(140,140,140)
	ELSE
		dw_3.Object.rama_grastd.Color = RGB(0,0,0)
		dw_3.Object.rama_grastd.Protect = 0
	END IF
	
	IF li_solfin=0 OR isnull(li_solfin) THEN
		dw_3.Object.rama_sofiin.Protect = 1
		dw_3.Object.rama_sofifi.Protect = 1
		dw_3.Object.rama_sofitd.Protect = 1
      dw_3.Setitem(1,"rama_sofiin",li_Null)
		dw_3.Setitem(1,"rama_sofifi",li_Null)
		dw_3.Setitem(1,"rama_sofitd",1)
		dw_3.Object.rama_sofitd.Color = RGB(140,140,140)
	ELSE
		dw_3.Object.rama_sofitd.Color = RGB(0,0,0)
		dw_3.Object.rama_sofitd.Protect = 0
	END IF
	
	IF li_matsec=0 OR isnull(li_matsec) THEN
		dw_3.Object.rama_matsin.Protect = 1
		dw_3.Object.rama_matsfi.Protect = 1
		dw_3.Object.rama_matstd.Protect = 1
      dw_3.Setitem(1,"rama_matsin",li_Null)
		dw_3.Setitem(1,"rama_matsfi",li_Null)
		dw_3.Setitem(1,"rama_matstd",1)
		dw_3.Object.rama_matstd.Color = RGB(140,140,140)
	ELSE
		dw_3.Object.rama_matstd.Color = RGB(0,0,0)
		dw_3.Object.rama_matstd.Protect = 0
	END IF
	
	IF li_nrosem=0 OR isnull(li_nrosem) THEN
		dw_3.Object.rama_nsemin.Protect = 1
		dw_3.Object.rama_nsemfi.Protect = 1
		dw_3.Object.rama_nsemtd.Protect = 1
      dw_3.Setitem(1,"rama_nsemin",li_Null)
		dw_3.Setitem(1,"rama_nsemfi",li_Null)
		dw_3.Setitem(1,"rama_nsemtd",1)
		dw_3.Object.rama_nsemtd.Color = RGB(140,140,140)
	ELSE
		dw_3.Object.rama_nsemtd.Color = RGB(0,0,0)
		dw_3.Object.rama_nsemtd.Protect = 0
	END IF
	
END IF	
end subroutine

event open;/* Argumentos

istr_mant.argumento[1] = Usuario
istr_mant.argumento[2] = Especie
istr_mant.argumento[3] = Variedad
istr_mant.argumento[4] = 
*/

istr_Mant.Argumento[1]	=	''
istr_Mant.Argumento[2]	=	''
istr_Mant.Argumento[3]	=	''
istr_Mant.Argumento[4]	=	''

x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

dw_2.GetChild("expo_codigo", idwc_exporta)
idwc_exporta.SetTransObject(SqlCa)
IF idwc_exporta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Exportador")
	idwc_exporta.InsertRow(0)
END IF

dw_2.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(SqlCa)
IF idwc_productor.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
END IF

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(SqlCa)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
END IF

dw_2.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(SqlCa)
IF idwc_variedad.Retrieve(gstr_paramplanta.codigoespecie) = 0 THEN
   idwc_variedad.InsertRow(0)
END IF	

dw_2.GetChild("cama_codigo", idwc_camara)
idwc_camara.SetTransObject(SqlCa)
idwc_camara.InsertRow(0)

dw_2.GetChild("grva_codigo", idwc_grupo)
idwc_grupo.SetTransObject(SqlCa)
IF idwc_grupo.Retrieve(gstr_paramplanta.codigoespecie,0) = 0 THEN
   idwc_grupo.InsertRow(0)
END IF	

dw_2.GetChild("grva_codsub", idwc_subgrupo)
idwc_subgrupo.SetTransObject(SqlCa)
IF idwc_subgrupo.Retrieve(gstr_paramplanta.codigoespecie,0) = 0 THEN
   idwc_subgrupo.InsertRow(0)
END IF

dw_2.GetChild("frio_tipofr", idwc_tipofrio)
idwc_tipofrio.SetTransObject(SqlCa)
IF idwc_tipofrio.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Tipo de Frío")
	idwc_tipofrio.InsertRow(0)
END IF

dw_2.GetChild("pefr_codigo", idwc_periodofrio)
idwc_periodofrio.SetTransObject(SqlCa)
IF idwc_periodofrio.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Periodos de Frío")
	idwc_periodofrio.InsertRow(0)
END IF

dw_3	=	tab_1.tp_1.dw_madurez
dw_4	=	tab_1.tp_2.dw_colorfondo
dw_5	=	tab_1.tp_3.dw_cubrim
dw_6	=	tab_1.tp_4.dw_calibre

dw_4.GetChild("cofo_codigo", idwc_color_fondo)
idwc_color_fondo.SetTransObject(SqlCa)
idwc_color_fondo.InsertRow(0)


dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

pb_nuevo.PostEvent(Clicked!)

iuo_productores		=	Create uo_productores
iuo_planta				=  Create uo_plantadesp
iuo_especie       	=  Create uo_especie
iuo_camarasfrigo		=	Create uo_camarasfrigo
iuo_grupoespecie		=	Create uo_grupoespecie
iuo_subgrupoespecie	=	Create uo_subgrupoespecie
iuo_variedades			=	Create uo_variedades
iuo_tratamientofrio	=	Create uo_tratamientofrio
iuo_periodofrio		=	Create uo_periodofrio
iuo_Madurez				=  Create uo_parammadurez

ids_Base				=	Create DataStore


end event

event ue_recuperadatos();
Long	ll_fila_d, ll_fila_e, respuesta
Integer li_grupo, li_planta


DO
	dw_2.SetRedraw(False)
	
	ll_fila_e	= dw_2.Retrieve(istr_mant.argumento[1], &
										 integer(istr_mant.argumento[2]))

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila_e > 0 THEN
		
		DO

			IF dw_3.Retrieve(istr_mant.argumento[1],&
								  Integer(istr_mant.argumento[2])) = -1 OR &
				dw_4.Retrieve(istr_mant.argumento[1],&
								  Integer(istr_mant.argumento[2])) = -1 OR &
				dw_5.Retrieve(istr_mant.argumento[1],&
								  Integer(istr_mant.argumento[2]))= -1 OR &
				dw_6.Retrieve(istr_mant.argumento[1],&
								  Integer(istr_mant.argumento[2]))= -1  THEN
				respuesta = MessageBox(	"Error en Base de Datos", &
								"No es posible conectar la Base de Datos.", &
								Information!, RetryCancel!)
			ELSE
				li_grupo = dw_2.Object.grva_codigo[1]
				IF isnull(li_grupo) THEN li_grupo=0 
				dw_2.GetChild("grva_codsub",idwc_subgrupo)
				idwc_subgrupo.SetTransObject(SQLCA)
				IF idwc_subgrupo.Retrieve(dw_2.Object.espe_codigo[1],li_grupo)=0 THEN
					idwc_subgrupo.InsertRow(0)
				END IF	

				/*  */
				dw_2.GetChild("vari_codigo", idwc_variedad)
				idwc_variedad.SetTransObject(SqlCa)
				IF idwc_variedad.Retrieve(dw_2.Object.espe_codigo[1]) = 0 THEN
					idwc_variedad.InsertRow(0)
				END IF

				li_planta = dw_2.Object.plde_codigo[1]
				IF NOT isnull(li_planta) THEN
					dw_2.GetChild("cama_codigo", idwc_camara)
					idwc_camara.SetTransObject(SqlCa)
					IF idwc_camara.Retrieve(li_planta) = 0 THEN
	   				idwc_camara.InsertRow(0)
					END IF
				END IF
				
				habilitaparametros(dw_2.Object.espe_codigo[1],dw_2.Object.vari_codigo[1], &
				                   dw_2.Object.grva_codigo[1],dw_2.Object.grva_codsub[1] )
            HabilitaColor()   
				pb_grabar.Enabled		= True
	
				dw_1.SetRow(1)
				dw_1.SelectRow(1,True)
				dw_1.SetFocus()

			END IF		
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_info_seleccion_fruta_a_proceso.create
int iCurrent
call super::create
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
end on

on w_info_seleccion_fruta_a_proceso.destroy
call super::destroy
destroy(this.tab_1)
end on

event resize;//
end event

event ue_nuevo();call super::ue_nuevo;
Long ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not ib_ok THEN RETURN

pb_grabar.Enabled			=	False

dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)

dw_2.SetRedraw(True)

dw_2.SetItem(1,"expo_codigo", gi_codexport)

dw_2.SetItem(1,"sepr_usumod",upper(gstr_us.nombre))
dw_2.SetItem(1,"espe_codigo",gstr_paramplanta.codigoespecie)
dw_2.SetItem(1,"sepr_fecini",Date(Today()))
dw_2.SetItem(1,"sepr_fecfin",Date(Today()))
dw_2.SetItem(1,"sepr_pltcon",0)
dw_2.SetItem(1,"sepr_camcon",0)
dw_2.SetItem(1,"sepr_resren",0)
	
istr_mant.argumento[1]	= upper(gstr_us.nombre)
istr_mant.argumento[2]	= string(gstr_paramplanta.codigoespecie)

dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()

dw_3.insertRow(0)
dw_2.InsertRow(0)

dw_2.SetItem(1,"expo_codigo", gi_codexport)
dw_2.SetItem(1,"sepr_usumod", gstr_us.nombre)
dw_2.SetItem(1,"espe_codigo", gstr_paramplanta.codigoespecie)
dw_2.SetItem(1,"sepr_fecini", Date(Today()))
dw_2.SetItem(1,"sepr_fecfin", Date(Today()))
dw_2.SetItem(1,"sepr_pltcon", 0)
dw_2.SetItem(1,"sepr_camcon", 0)
dw_2.SetItem(1,"sepr_resren", 0)

IF NOT existeinforme("","") THEN
	Generadetalle(TRUE)
ELSE	
	TriggerEvent("ue_recuperadatos")
	Generadetalle(FALSE)
END IF	

pb_grabar.Enabled = TRUE

dw_2.Setcolumn("espe_codigo")
dw_2.SetFocus()


end event

event close;call super::close;//
end event

event ue_antesguardar;call super::ue_antesguardar;Long    ll_fila, ll_bultos, ll_productor
Integer li_especie, li_planta, li_camara, li_periodo, li_Nivel, li_pormin, &
        li_inicio, li_final
String  ls_usuario, ls_frio

li_Especie		=	dw_2.Object.espe_codigo[1]
ls_usuario		=	dw_2.Object.sepr_usumod[1]

IF dw_2.Object.sepr_plttod[1] = 0 THEN
	li_planta = dw_2.Object.plde_codigo[1]
	IF li_planta=0 or isnull(li_planta) THEN
		messagebox("Error de Datos","Debe Seleccionar una Planta o Todas.")
		Message.DoubleParm = -1
		Return
	END IF
END IF	

IF dw_2.Object.sepr_camtod[1] = 0 THEN
	li_camara = dw_2.Object.cama_codigo[1]
	IF isnull(li_camara) THEN
		messagebox("Error de Datos","Debe Seleccionar una Camara o Todas.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

IF dw_2.Object.sepr_protod[1] = 0 THEN
	ll_productor = dw_2.Object.prod_codigo[1]
	IF isnull(ll_productor) THEN
		messagebox("Error de Datos","Debe Seleccionar un Productor o Todos.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

IF dw_2.Object.sepr_fritod[1] = 0 THEN
	ls_frio = dw_2.Object.frio_tipofr[1]
	IF isnull(ls_frio) or ls_frio="" THEN
		messagebox("Error de Datos","Debe Seleccionar un Tipo de Frío o Todos.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

IF dw_2.Object.sepr_pertod[1] = 0 THEN
	li_periodo = dw_2.Object.pefr_codigo[1]
	IF isnull(li_periodo) or li_periodo = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Periodo de Frío o Todos.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

IF dw_2.Object.sepr_nivtod[1] = 0 THEN
	li_nivel = dw_2.Object.sepr_nivcal[1]
	IF isnull(li_nivel) or li_nivel = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Nivel de Calidad o Todos.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

IF dw_2.Object.sepr_bultod[1] = 0 THEN
	ll_bultos = dw_2.Object.sepr_bulmin[1]
	IF isnull(ll_bultos) or ll_bultos = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Nivel de Calidad o Todos.")
		Message.DoubleParm = -1
		Return
	END IF
	IF ll_bultos > 9999 THEN
		messagebox("Error de Datos","Los Bultos Mínimos no pueden superar los 9.999 Bultos.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

IF dw_2.Object.sepr_embtod[1] = 0 THEN
	li_pormin = dw_2.Object.sepr_pormin[1]
	IF isnull(li_pormin) or li_pormin = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Porcentaje Mínimo de Embalaje por Productor o Todos.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_pormin > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Mínimo de Embalaje por Productor no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

dw_3.Object.sepr_usumod[1] = ls_usuario
dw_3.Object.espe_codigo[1] = li_especie

li_inicio	=	0
li_final		=	0
IF dw_3.Object.rama_ssoltd[1] = 0 THEN
	li_inicio = dw_3.Object.rama_ssolin[1]
	li_final  = dw_3.Object.rama_ssolfi[1]
	IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y Final para los Solidos Solubles.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Inicial de lo Solidos Solubles no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_final > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Final de lo Solidos Solubles no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > li_Final THEN
		messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al Porcentaje Final de lo Solidos Solubles.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

li_inicio	=	0
li_final		=	0
IF dw_3.Object.rama_preetd[1] = 0 THEN
	li_inicio = dw_3.Object.rama_preein[1]
	li_final  = dw_3.Object.rama_preefi[1]
	IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y Final para la Presión Ecuatorial.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Inicial de la Presión Ecuatorial no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_final > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Final de la Presión Ecuatorial no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > li_Final THEN
		messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al Porcentaje Final de la Presión Ecuatorial.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

li_inicio	=	0
li_final		=	0
IF dw_3.Object.rama_prehtd[1] = 0 THEN
	li_inicio = dw_3.Object.rama_prehin[1]
	li_final  = dw_3.Object.rama_prehfi[1]
	IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y Final para la Presión Hombros.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Inicial de la Presión Hombros no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_final > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Final de la Presión Hombros no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > li_Final THEN
		messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al Porcentaje Final de la Presión Hombros.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

li_inicio	=	0
li_final		=	0
IF dw_3.Object.rama_preatd[1] = 0 THEN
	li_inicio = dw_3.Object.rama_preain[1]
	li_final  = dw_3.Object.rama_preafi[1]
	IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y Final para la Presión Apice.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Inicial de la Presión Apice no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_final > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Final de la Presión Apice no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > li_Final THEN
		messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al Porcentaje Final de la Presión Apice.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

li_inicio	=	0
li_final		=	0
IF dw_3.Object.rama_talmtd[1] = 0 THEN
	li_inicio = dw_3.Object.rama_talmin[1]
	li_final  = dw_3.Object.rama_talmfi[1]
	IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y Final para el Test de Almidón.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Inicial del Test de Almidón no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_final > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Final del Test de Almidón no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > li_Final THEN
		messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al Porcentaje Final del Test de Almidón.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

li_inicio	=	0
li_final		=	0
IF dw_3.Object.rama_acidtd[1] = 0 THEN
	li_inicio = dw_3.Object.rama_acidin[1]
	li_final  = dw_3.Object.rama_acidfi[1]
	IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y Final para la Acidez.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Inicial de la Acidez no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_final > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Final de la Acididez no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > li_Final THEN
		messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al Porcentaje Final de la Acidez.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

li_inicio	=	0
li_final		=	0
IF dw_3.Object.rama_sofitd[1] = 0 THEN
	li_inicio = dw_3.Object.rama_sofiin[1]
	li_final  = dw_3.Object.rama_sofifi[1]
	IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y Final para los Solidos Finales.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Inicial de los Solidos Finales no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_final > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Final de los Solidos Finales no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > li_Final THEN
		messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al Porcentaje Final de los Solidos Finales.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

li_inicio	=	0
li_final		=	0
IF dw_3.Object.rama_matstd[1] = 0 THEN
	li_inicio = dw_3.Object.rama_matsin[1]
	li_final  = dw_3.Object.rama_matsfi[1]
	IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y Final para la Materia Seca.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Inicial de la Materia Seca no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_final > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Final de la Materia Seca no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > li_Final THEN
		messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al Porcentaje Final de la Materia Seca.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

li_inicio	=	0
li_final		=	0
IF dw_3.Object.rama_nsemtd[1] = 0 THEN
	li_inicio = dw_3.Object.rama_nsemin[1]
	li_final  = dw_3.Object.rama_nsemfi[1]
	IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Rango Inicial y un Rango Final para el Número de Semillas.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > 999 THEN
		messagebox("Error de Datos","El Rango Inicial del Número de Semillas no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_final > 999 THEN
		messagebox("Error de Datos","El Rango Final del Número de Semillas no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > li_Final THEN
		messagebox("Error de Datos","El Rango Inicial debe ser menor al Rango Final del Número de Semillas.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

li_inicio	=	0
li_final		=	0
IF dw_3.Object.rama_coactd[1] = 0 THEN
	li_inicio = dw_3.Object.rama_coacin[1]
	li_final  = dw_3.Object.rama_coacfi[1]
	IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y un Porcentaje Final para el Corazón Acuoso.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Inicial del Corazón Acuoso no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_final > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Final del Corazón Acuoso no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > li_Final THEN
		messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al porcentaje Final del Corazón Acuoso.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

li_inicio	=	0
li_final		=	0
IF dw_3.Object.rama_comotd[1] = 0 THEN
	li_inicio = dw_3.Object.rama_comoin[1]
	li_final  = dw_3.Object.rama_comofi[1]
	IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y un Porcentaje Final para el Corazón Mohoso.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Inicial del Corazón Mohoso no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_final > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Final del Corazón Mohoso no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > li_Final THEN
		messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al porcentaje Final del Corazón Mohoso.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

li_inicio	=	0
li_final		=	0
IF dw_3.Object.rama_haritd[1] = 0 THEN
	li_inicio = dw_3.Object.rama_hariin[1]
	li_final  = dw_3.Object.rama_harifi[1]
	IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y un Porcentaje Final para la Harinosidad.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Inicial de la Harinosidad no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_final > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Final de la Harinosidad no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > li_Final THEN
		messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al porcentaje Final de la Harinosidad.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

li_inicio	=	0
li_final		=	0
IF dw_3.Object.rama_grastd[1] = 0 THEN
	li_inicio = dw_3.Object.rama_grasin[1]
	li_final  = dw_3.Object.rama_grasfi[1]
	IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
		messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y un Porcentaje Final para la Grasitud.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Inicial de la Grasitud no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_final > 99.99 THEN
		messagebox("Error de Datos","El Porcentaje Final de la Grasitud no puede superar el 99.99%.")
		Message.DoubleParm = -1
		Return
	END IF
	IF li_inicio > li_Final THEN
		messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al Porcentaje Final de la Grasitud.")
		Message.DoubleParm = -1
		Return
	END IF
END IF

FOR ll_Fila = 1 TO dw_4.RowCount()
	IF dw_4.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		IF dw_4.Object.raco_todos[ll_fila] = 0 THEN
			li_inicio = dw_4.Object.raco_inicia[ll_fila]
			li_final  = dw_4.Object.raco_finali[ll_Fila]
			IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
				messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y un Porcentaje Final para la Fila "+string(ll_fila)+" del Color de Fondo.")
				Message.DoubleParm = -1
				Return
			END IF
			IF li_inicio > 99.99 THEN
				messagebox("Error de Datos","El Porcentaje Inicial de la Fila "+string(ll_fila)+" del Color de Fondo. No puede superar el 99.99%.")
				Message.DoubleParm = -1
				Return
			END IF
			IF li_final > 99.99 THEN
				messagebox("Error de Datos","El Porcentaje Final de la Fila "+string(ll_fila)+" del Color de Fondo. No puede superar el 99.99%.")
				Message.DoubleParm = -1
				Return
			END IF
			IF li_inicio > li_Final THEN
				messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al Porcentaje Final de la Fila "+string(ll_fila)+" del Color de Fondo.")
				Message.DoubleParm = -1
				Return
			END IF
		END IF
		dw_4.Object.sepr_usumod[ll_fila] = ls_usuario
		dw_4.Object.espe_codigo[ll_fila] = li_especie
	END IF
NEXT

FOR ll_Fila = 1 TO dw_5.RowCount()
	
	IF dw_5.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		IF dw_5.Object.racu_todos[ll_fila] = 0 THEN
			li_inicio = dw_5.Object.racu_inicia[ll_fila]
			li_final  = dw_5.Object.racu_finali[ll_Fila]
			IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
				messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y un Porcentaje Final para la Fila "+string(ll_fila)+" del Color de Cubrimiento.")
				Message.DoubleParm = -1
				Return
			END IF
			IF li_inicio > 99.99 THEN
				messagebox("Error de Datos","El Porcentaje Inicial de la Fila "+string(ll_fila)+" del Color de Cubrimiento. No puede superar el 99.99%.")
				Message.DoubleParm = -1
				Return
			END IF
			IF li_final > 99.99 THEN
				messagebox("Error de Datos","El Porcentaje Final de la Fila "+string(ll_fila)+" del Color de Cubrimiento. No puede superar el 99.99%.")
				Message.DoubleParm = -1
				Return
			END IF
			IF li_inicio > li_Final THEN
				messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al Porcentaje Final de la Fila "+string(ll_fila)+" del Color de Cubrimiento.")
				Message.DoubleParm = -1
				Return
			END IF
		END IF
		dw_5.Object.sepr_usumod[ll_fila] = ls_usuario
		dw_5.Object.espe_codigo[ll_fila] = li_especie
	END IF
NEXT

FOR ll_Fila = 1 TO dw_6.RowCount()
	
	IF dw_6.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		IF dw_6.Object.raca_todos[ll_fila] = 0 THEN
			li_inicio = dw_6.Object.raca_inicia[ll_fila]
			li_final  = dw_6.Object.raca_finali[ll_Fila]
			IF isnull(li_inicio) or li_inicio = 0 or isnull(li_final) or li_final = 0 THEN
				messagebox("Error de Datos","Debe Seleccionar un Porcentaje Inicial y un Porcentaje Final para la Fila "+string(ll_fila)+" de la Distribución de Calibres.")
				Message.DoubleParm = -1
				Return
			END IF
			IF li_inicio > 99.99 THEN
				messagebox("Error de Datos","El Porcentaje Inicial de la Fila "+string(ll_fila)+" de la Distribución de Calibres. No puede superar el 99.99%.")
				Message.DoubleParm = -1
				Return
			END IF
			IF li_final > 99.99 THEN
				messagebox("Error de Datos","El Porcentaje Final de la Fila "+string(ll_fila)+" de la Distribución de Calibres. No puede superar el 99.99%.")
				Message.DoubleParm = -1
				Return
			END IF
			IF li_inicio > li_Final THEN
				messagebox("Error de Datos","El Porcentaje Inicial debe ser menor al Porcentaje Final de la Fila "+string(ll_fila)+" de la Distribución de Calibres.")
				Message.DoubleParm = -1
				Return
			END IF
		END IF
		dw_6.Object.sepr_usumod[ll_fila] = ls_usuario
		dw_6.Object.espe_codigo[ll_fila] = li_especie
	END IF
NEXT


end event

event ue_borrar();IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)
IF dw_4.RowCount() > 0 THEN dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)
IF dw_5.RowCount() > 0 THEN dw_5.RowsMove(1,dw_5.RowCount(),Primary!,dw_5,1,Delete!)
IF dw_6.RowCount() > 0 THEN dw_6.RowsMove(1,dw_6.RowCount(),Primary!,dw_6,1,Delete!)
IF dw_7.RowCount() > 0 THEN dw_7.RowsMove(1,dw_7.RowCount(),Primary!,dw_7,1,Delete!)
IF dw_9.RowCount() > 0 THEN dw_9.RowsMove(1,dw_9.RowCount(),Primary!,dw_9,1,Delete!)

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

event ue_guardar();IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	imprime_seleccion()
   TriggerEvent("ue_imprimir")
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type dw_1 from w_mant_encab_deta`dw_1 within w_info_seleccion_fruta_a_proceso
boolean visible = false
integer x = 91
integer y = 960
integer width = 1061
integer height = 928
boolean enabled = false
string title = ""
end type

type dw_2 from w_mant_encab_deta`dw_2 within w_info_seleccion_fruta_a_proceso
integer x = 41
integer y = 40
integer width = 3173
integer height = 804
integer taborder = 10
string dataobject = "dw_mant_mues_selecfrutaproc"
end type

event dw_2::itemchanged;String	ls_Null
String	ls_columna

ls_columna = GetColumnName()
SetNull(ls_Null)

CHOOSE CASE ls_columna

  CASE "sepr_plttod"
	   IF data="1" tHEN
			dw_2.SetItem(1,"plde_codigo",integer(ls_null))
			dw_2.SetItem(1,"cama_codigo",integer(ls_null))
			dw_2.Object.plde_codigo.Protect = 1
			dw_2.Object.cama_codigo.Protect = 1
			dw_2.Object.plde_codigo.BackGround.Color = RGB(192,192,192)
			dw_2.Object.cama_codigo.BackGround.Color = RGB(192,192,192)
		ELSE
			dw_2.Object.plde_codigo.Protect = 0
			dw_2.Object.plde_codigo.BackGround.Color = RGB(255,255,255)
			dw_2.SetItem(1,"sepr_pltcon",0)
			dw_2.SetColumn("plde_codigo")
			dw_2.SetFocus()
		END IF	
	
  CASE "sepr_camtod"
	   IF data="1" tHEN
			dw_2.SetItem(1,"cama_codigo",integer(ls_null))
			dw_2.Object.cama_codigo.Protect = 1
			dw_2.Object.cama_codigo.BackGround.Color = RGB(192,192,192)
		ELSE
			dw_2.Object.cama_codigo.Protect = 0
			dw_2.Object.cama_codigo.BackGround.Color = RGB(255,255,255)
			dw_2.SetItem(1,"sepr_camcon",0)
			dw_2.SetColumn("cama_codigo")
			dw_2.SetFocus()
		END IF

  CASE "sepr_protod"
	   IF data="1" tHEN
			dw_2.SetItem(1,"prod_codigo",long(ls_null))
			dw_2.Object.prod_codigo.Protect = 1
			dw_2.Object.prod_codigo.BackGround.Color = RGB(192,192,192)
		ELSE
			dw_2.Object.prod_codigo.Protect = 0
			dw_2.Object.prod_codigo.BackGround.Color = RGB(255,255,255)
			dw_2.SetColumn("prod_codigo")
			dw_2.SetFocus()
		END IF
	
  CASE "sepr_fritod"
	   IF data="1" tHEN
			dw_2.SetItem(1,"frio_tipofr",ls_null)
			dw_2.Object.frio_tipofr.Protect = 1
			dw_2.Object.frio_tipofr.BackGround.Color = RGB(192,192,192)
		ELSE
			dw_2.Object.frio_tipofr.Protect = 0
			dw_2.Object.frio_tipofr.BackGround.Color = RGB(255,255,255)
			dw_2.SetColumn("frio_tipofr")
			dw_2.SetFocus()
		END IF
	
  CASE "sepr_pertod"
	   IF data="1" tHEN
			dw_2.SetItem(1,"pefr_codigo",integer(ls_null))
			dw_2.Object.pefr_codigo.Protect = 1
			dw_2.Object.pefr_codigo.BackGround.Color = RGB(192,192,192)
		ELSE
			dw_2.Object.pefr_codigo.Protect = 0
			dw_2.Object.pefr_codigo.BackGround.Color = RGB(255,255,255)
			dw_2.SetColumn("pefr_codigo")
			dw_2.SetFocus()
		END IF
	
  CASE "sepr_bultod"
	   IF data="1" tHEN
			dw_2.SetItem(1,"sepr_bulmin",integer(ls_null))
			dw_2.Object.sepr_bulmin.Protect = 1
			dw_2.Object.sepr_bulmin.BackGround.Color = RGB(192,192,192)
		ELSE
			dw_2.Object.sepr_bulmin.Protect = 0
			dw_2.Object.sepr_bulmin.BackGround.Color = RGB(255,255,255)
			dw_2.SetColumn("sepr_bulmin")
			dw_2.SetFocus()
		END IF
	
	CASE "sepr_embtod"
	   IF data="1" tHEN
			dw_2.SetItem(1,"sepr_pormin",integer(ls_null))
			dw_2.Object.sepr_pormin.Protect = 1
			dw_2.Object.sepr_pormin.BackGround.Color = RGB(192,192,192)
		ELSE
			dw_2.Object.sepr_pormin.Protect = 0
			dw_2.Object.sepr_pormin.BackGround.Color = RGB(255,255,255)
			dw_2.SetColumn("sepr_pormin")
			dw_2.SetFocus()
		END IF
	
	
	CASE "sepr_nivtod"
	   IF data="1" tHEN
			dw_2.SetItem(1,"sepr_nivcal",integer(ls_null))
			dw_2.Object.sepr_nivcal.Protect = 1
			dw_2.Object.sepr_nivcal.BackGround.Color = RGB(192,192,192)
		ELSE
			dw_2.Object.sepr_nivcal.Protect = 0
			dw_2.Object.sepr_nivcal.BackGround.Color = RGB(255,255,255)
			dw_2.SetColumn("sepr_nivcal")
			dw_2.SetFocus()
		END IF
		
	CASE "sepr_pltcon"
	   IF data="1" tHEN
			dw_2.SetItem(1,"plde_codigo",integer(ls_null))
			dw_2.SetItem(1,"cama_codigo",integer(ls_null))			
			dw_2.SetItem(1,"sepr_plttod",1)
			dw_2.SetItem(1,"sepr_camtod",1)
			dw_2.Object.plde_codigo.Protect = 1
			dw_2.Object.cama_codigo.Protect = 1
			dw_2.Object.plde_codigo.BackGround.Color = RGB(192,192,192)
			dw_2.Object.cama_codigo.BackGround.Color = RGB(192,192,192)

		END IF
		
	CASE "sepr_camcon"
	   IF data="1" tHEN
			dw_2.SetItem(1,"cama_codigo",integer(ls_null))
			dw_2.SetItem(1,"sepr_camtod",1)
			dw_2.Object.cama_codigo.Protect = 1
			dw_2.Object.cama_codigo.BackGround.Color = RGB(192,192,192)
		END IF	
		
	CASE "plde_codigo"
	  IF iuo_planta.existe(integer(data),True,Sqlca) THEN
	     this.getchild("cama_codigo",idwc_camara)
		  idwc_camara.SetTransObject(SQLCA)
		  IF idwc_camara.Retrieve(integer(data))=0 THEN
			  idwc_camara.insertRow(0)
		  END IF  
     END IF	
	  
	CASE "espe_codigo" 
		istr_mant.argumento[2] = ""
		istr_mant.argumento[3] = ""
		This.SetItem(1, "espe_codigo", integer(ls_Null))
		This.SetItem(1, "grva_codigo", integer(ls_Null))
		This.SetItem(1, "grva_codsub", integer(ls_Null))
		This.SetItem(1, "vari_codigo", integer(ls_Null))
		IF Not iuo_especie.existe(integer(data),True,SQLCA) THEN
			This.SetItem(1, "espe_codigo", integer(ls_Null))
			This.SetItem(1, "grva_codigo", integer(ls_Null))
			This.SetItem(1, "grva_codsub", integer(ls_Null))
			This.SetItem(1, "vari_codigo", integer(ls_Null))
			dw_2.Object.grva_codsub.Protect=1
			dw_2.Object.grva_codsub.BackGround.Color = RGB(192,192,192)
			dw_2.Object.grva_codigo.Protect=1
			dw_2.Object.grva_codigo.BackGround.Color = RGB(192,192,192)
			dw_2.Object.vari_codigo.Protect=1
			dw_2.Object.vari_codigo.BackGround.Color = RGB(192,192,192)
			This.SetFocus()
			RETURN 1
		ELSE
			istr_mant.argumento[2] = data
			dw_2.Object.grva_codigo.Protect=0
			dw_2.Object.grva_codigo.BackGround.Color = RGB(255,255,255)
			
			dw_2.Object.grva_codsub.Protect=0
			dw_2.Object.grva_codsub.BackGround.Color = RGB(255,255,255)
			
			dw_3.SetReDraw(FALSE)
			dw_4.SetReDraw(FALSE)
			dw_5.SetReDraw(FALSE)
			dw_6.SetReDraw(FALSE)
						
			dw_4.Reset()
			dw_5.Reset()
			dw_6.Reset()
			IF NOT existeinforme("espe_codigo",Data) THEN
				dw_2.SetReDraw(FALSE)
				dw_2.Reset()   
				dw_3.Reset()
				dw_2.InsertRow(0)
				dw_3.InsertRow(0)
				dw_2.SetItem(1,"expo_codigo", gi_codexport)
				dw_2.SetItem(1,"sepr_usumod", gstr_us.nombre)
				dw_2.SetItem(1,"espe_codigo", Integer(data))
				dw_2.SetItem(1,"sepr_fecini", Date(Today()))
				dw_2.SetItem(1,"sepr_fecfin", Date(Today()))
				dw_2.SetItem(1,"sepr_pltcon", 0)
				dw_2.SetItem(1,"sepr_camcon", 0)
				dw_2.SetItem(1,"sepr_resren", 0)
				
				dw_2.GetChild("grva_codigo",idwc_grupo)
				idwc_grupo.SetTransObject(SQLCA)
				IF idwc_grupo.Retrieve(Integer(data),0) = 0 THEN
					idwc_grupo.InsertRow(0)
				END IF	
				
				This.Object.grva_codigo[row]		=	integer(ls_Null)
				/*  */
				dw_2.GetChild("grva_codsub",idwc_subgrupo)
				idwc_subgrupo.SetTransObject(SQLCA)
				IF idwc_subgrupo.Retrieve(Integer(data),0) = 0 THEN
					idwc_subgrupo.InsertRow(0)
				END IF	
				
				This.Object.grva_codsub[row]	=	integer(ls_Null)
				
				/*  */
				This.GetChild("vari_codigo", idwc_variedad)
				idwc_variedad.SetTransObject(SqlCa)
				IF idwc_variedad.Retrieve(Integer(data)) = 0 THEN
					idwc_variedad.InsertRow(0)
				END IF

				Generadetalle(TRUE)
				dw_2.SetReDraw(TRUE)
				habilitaparametros(integer(data),dw_2.Object.vari_codigo[1],dw_2.Object.grva_codigo[1],&
			                   dw_2.Object.grva_codsub[1])
			ELSE
				parent.TriggerEvent("ue_recuperadatos")
				Generadetalle(FALSE)
			END IF
			
			dw_3.SetReDraw(TRUE)
			dw_4.SetReDraw(TRUE)
			dw_5.SetReDraw(TRUE)
			dw_6.SetReDraw(TRUE)
		END IF
	
	
	CASE "cama_codigo"
		IF NOT iuo_CamarasFrigo.Existe(This.Object.plde_codigo[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "cama_codigo", integer(ls_Null) )
			This.SetFocus()
			RETURN 1
		END IF

	CASE "vari_codigo"
		IF Data <>'' THEN
			istr_mant.argumento[3] = ""
			IF NOT iuo_Variedades.Existe(This.Object.espe_codigo[row],Integer(data),True,SqlCa) THEN
				This.SetItem(1, "vari_codigo", integer(ls_Null) )
				This.SetFocus()
				RETURN 1
			ELSE
				dw_2.Object.grva_codigo.Protect=0
				dw_2.Object.grva_codigo.BackGround.Color = RGB(255,255,255)
				
				dw_2.Object.grva_codsub.Protect=0
				dw_2.Object.grva_codsub.BackGround.Color = RGB(255,255,255)
				
				This.Object.grva_codigo[row]	=	iuo_Variedades.Grupo
				dw_2.GetChild("grva_codsub",idwc_subgrupo)
				idwc_subgrupo.SetTransObject(SQLCA)
				IF idwc_subgrupo.Retrieve(dw_2.Object.espe_codigo[1],iuo_Variedades.Grupo)=0 THEN
					idwc_subgrupo.InsertRow(0)
				END IF	
				This.Object.grva_codsub[row]	=	iuo_Variedades.SubGrupo
				
				istr_mant.argumento[3] = data
				
				IF eliminadatos() THEN
					dw_4.Reset()
					dw_5.Reset()
					dw_6.Reset()
				
					Generadetalle(TRUE)
				END IF	
				habilitaparametros(dw_2.Object.espe_codigo[1],integer(data),dw_2.Object.grva_codigo[1],&
   			                   dw_2.Object.grva_codsub[1])

			END IF 
		ELSE
			THIS.SetItem(1, "vari_codigo", integer(ls_Null) )
			THIS.SetFocus()
			RETURN 1
		END IF
		
	CASE "grva_codigo"
	 IF data<>"" THEN	
		istr_mant.argumento[3] = ""
		IF NOT iuo_GrupoEspecie.Existe(This.Object.espe_codigo[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "grva_codigo", integer(ls_Null))
			This.SetItem(1, "grva_codsub", integer(ls_Null))
			This.SetItem(1, "vari_codigo", integer(ls_Null))
			dw_4.Reset()
			dw_2.Object.grva_codsub.Protect=1
			dw_2.Object.grva_codsub.BackGround.Color = RGB(192,192,192)
			This.SetFocus()
			RETURN 1
		ELSE
			dw_4.Reset()
			dw_2.Object.grva_codsub.Protect=0
			dw_2.Object.grva_codsub.BackGround.Color = RGB(255,255,255)
			
			This.Object.grva_codsub[row]	=	integer(ls_Null)
			This.SetItem(1, "vari_codigo", integer(ls_Null))			
			
			idwc_subgrupo.Retrieve(This.Object.espe_codigo[row],Integer(data))

			habilitaparametros(dw_2.Object.espe_codigo[1],dw_2.Object.vari_codigo[1],integer(data),&
   		                   dw_2.Object.grva_codsub[1])

		END IF
	ELSE
		This.SetItem(1, "grva_codigo", integer(ls_Null))
		This.SetItem(1, "grva_codsub", integer(ls_Null))
		This.SetItem(1, "vari_codigo", integer(ls_Null))
		dw_2.Object.grva_codsub.Protect=1
		dw_2.Object.grva_codsub.BackGround.Color = RGB(192,192,192)
		habilitaparametros(dw_2.Object.espe_codigo[1],dw_2.Object.vari_codigo[1],integer(data),&
  		                   dw_2.Object.grva_codsub[1])
		This.SetFocus()
		RETURN 1
	END IF		
	
	CASE "subgrupo"
		IF data<>"" THEN
			istr_mant.argumento[3] = ""
			IF NOT iuo_SubGrupoEspecie.Existe(This.Object.espe_codigo[row],&
				This.Object.grva_codigo[row],Integer(data),True,SqlCa) THEN
				This.SetItem(1, "vari_codigo", integer(ls_Null))
				This.SetItem(1, "grva_codsub", integer(ls_Null))
				dw_4.Reset()
				This.SetFocus()
				RETURN 1
			ELSE
				dw_4.Reset()
				habilitaparametros(dw_2.Object.espe_codigo[1],dw_2.Object.vari_codigo[1],dw_2.Object.grva_codigo, &
										 integer(data))
	
			END IF
		ELSE
			This.SetItem(1, "grva_codsub", integer(ls_Null) )
			This.SetItem(1, "vari_codigo", integer(ls_Null))
			habilitaparametros(dw_2.Object.espe_codigo[1],dw_2.Object.vari_codigo[1],dw_2.Object.grva_codigo, &
									 integer(data))
			This.SetFocus()
		END IF
		
	CASE "frio_tipofr"
		IF NOT iuo_TratamientoFrio.Ofp_Recupera_TratamientoFrio(SqlCa,data,True) THEN
			This.SetItem(1, "frio_tipofr", ls_Null)
			This.SetFocus()
			RETURN 1
		END IF

	CASE "pefr_codigo"
		IF NOT iuo_PeriodoFrio.Ofp_Recupera_PeriodoFrio(SqlCa,Integer(data),True) THEN
			This.SetItem(1, "pefr_codigo", integer(ls_Null))
			This.SetFocus()
			RETURN 1
		END IF
	
	
	CASE "prod_codigo"
		IF NOT iuo_Productores.Existe(Long(data),True,SqlCa) THEN
			This.SetItem(1, "prod_codigo", long(ls_Null))
			This.SetFocus()
			RETURN 1
		END IF
		
		
END CHOOSE

end event

event dw_2::doubleclicked;//
end event

event dw_2::buttonclicked;call super::buttonclicked;//CHOOSE CASE dwo.Name
//	CASE "busca_productor"
//		BuscaProductor()
//
//END CHOOSE

end event

event dw_2::itemerror;RETURN 1
end event

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_info_seleccion_fruta_a_proceso
boolean visible = false
integer x = 3287
integer taborder = 40
end type

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_info_seleccion_fruta_a_proceso
boolean visible = false
integer x = 3287
integer taborder = 50
end type

type pb_grabar from w_mant_encab_deta`pb_grabar within w_info_seleccion_fruta_a_proceso
integer x = 3282
integer y = 772
integer taborder = 60
string picturename = "\desarrollo\bmp\imprimee.bmp"
string disabledname = "\desarrollo\bmp\imprimed.bmp"
end type

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_info_seleccion_fruta_a_proceso
boolean visible = false
integer x = 3287
integer taborder = 70
end type

type pb_salir from w_mant_encab_deta`pb_salir within w_info_seleccion_fruta_a_proceso
integer x = 3287
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_info_seleccion_fruta_a_proceso
boolean visible = false
integer x = 3287
integer y = 1504
integer taborder = 0
end type

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_info_seleccion_fruta_a_proceso
boolean visible = false
integer x = 3287
integer y = 1680
integer taborder = 0
end type

type pb_buscar from w_mant_encab_deta`pb_buscar within w_info_seleccion_fruta_a_proceso
boolean visible = false
integer x = 3287
integer taborder = 30
end type

type tab_1 from tab within w_info_seleccion_fruta_a_proceso
event create ( )
event destroy ( )
integer x = 37
integer y = 864
integer width = 3173
integer height = 1028
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean multiline = true
boolean raggedright = true
boolean focusonbuttondown = true
boolean powertips = true
integer selectedtab = 1
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
tp_4 tp_4
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_2=create tp_2
this.tp_3=create tp_3
this.tp_4=create tp_4
this.Control[]={this.tp_1,&
this.tp_2,&
this.tp_3,&
this.tp_4}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_2)
destroy(this.tp_3)
destroy(this.tp_4)
end on

type tp_1 from userobject within tab_1
event create ( )
event destroy ( )
string tag = "Antecedentes de Madurez de Cosecha"
integer x = 18
integer y = 112
integer width = 3136
integer height = 900
long backcolor = 12632256
string text = "Madurez Cosecha"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Environment!"
long picturemaskcolor = 553648127
dw_madurez dw_madurez
end type

on tp_1.create
this.dw_madurez=create dw_madurez
this.Control[]={this.dw_madurez}
end on

on tp_1.destroy
destroy(this.dw_madurez)
end on

type dw_madurez from datawindow within tp_1
integer x = 32
integer y = 36
integer width = 2999
integer height = 848
integer taborder = 10
boolean bringtotop = true
string dataobject = "dw_mant_mues_rangosmadurez_sel"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;IF Row > 0 THEN
	This.SelectRow(0,False)
	This.SetRow(row)
	
END IF
end event

event rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event losefocus;Accepttext()
end event

event itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event itemerror;RETURN 1
end event

event itemchanged;String	ls_columna, ls_Null
Integer	li_color
Long		ll_FilaDano

SetNull(ls_Null)

ls_columna = GetColumnName()

CHOOSE CASE ls_columna
		
	CASE "rama_ssoltd"
		IF data = '0' THEN
		   this.object.rama_ssolin.BackGround.Color = RGB(255,255,255)
			this.object.rama_ssolfi.BackGround.Color = RGB(255,255,255)
		ELSE
			this.SetItem(1,'rama_ssolin',integer(ls_Null))
			this.SetItem(1,'rama_ssolfi',integer(ls_Null))
			this.object.rama_ssolin.BackGround.Color = RGB(192,192,192)
			this.object.rama_ssolfi.BackGround.Color = RGB(192,192,192)
		END IF	

	CASE "rama_preetd"
		IF data = '0' THEN
		   this.object.rama_preein.BackGround.Color = RGB(255,255,255)
			this.object.rama_preefi.BackGround.Color = RGB(255,255,255)
		ELSE
			this.SetItem(1,'rama_preein',integer(ls_Null))
			this.SetItem(1,'rama_preefi',integer(ls_Null))
			this.object.rama_preein.BackGround.Color = RGB(192,192,192)
			this.object.rama_preefi.BackGround.Color = RGB(192,192,192)
		END IF	
		
	CASE "rama_prehtd"
		IF data = '0' THEN
		   this.object.rama_prehin.BackGround.Color = RGB(255,255,255)
			this.object.rama_prehfi.BackGround.Color = RGB(255,255,255)
		ELSE
			this.SetItem(1,'rama_prehin',integer(ls_Null))
			this.SetItem(1,'rama_prehfi',integer(ls_Null))
			this.object.rama_prehin.BackGround.Color = RGB(192,192,192)
			this.object.rama_prehfi.BackGround.Color = RGB(192,192,192)
		END IF
		
	CASE "rama_preatd"
		IF data = '0' THEN
		   this.object.rama_preain.BackGround.Color = RGB(255,255,255)
			this.object.rama_preafi.BackGround.Color = RGB(255,255,255)
		ELSE
			this.SetItem(1,'rama_preain',integer(ls_Null))
			this.SetItem(1,'rama_preafi',integer(ls_Null))
			this.object.rama_preain.BackGround.Color = RGB(192,192,192)
			this.object.rama_preafi.BackGround.Color = RGB(192,192,192)
		END IF	
		
	CASE "rama_talmtd"
		IF data = '0' THEN
		   this.object.rama_talmin.BackGround.Color = RGB(255,255,255)
			this.object.rama_talmfi.BackGround.Color = RGB(255,255,255)
		ELSE
			this.SetItem(1,'rama_talmin',integer(ls_Null))
			this.SetItem(1,'rama_talmfi',integer(ls_Null))
			this.object.rama_talmin.BackGround.Color = RGB(192,192,192)
			this.object.rama_talmfi.BackGround.Color = RGB(192,192,192)
		END IF	
		
	CASE "rama_acidtd"
		IF data = '0' THEN
		   this.object.rama_acidin.BackGround.Color = RGB(255,255,255)
			this.object.rama_acidfi.BackGround.Color = RGB(255,255,255)
		ELSE
			this.SetItem(1,'rama_acidin',integer(ls_Null))
			this.SetItem(1,'rama_acidfi',integer(ls_Null))
			this.object.rama_acidin.BackGround.Color = RGB(192,192,192)
			this.object.rama_acidfi.BackGround.Color = RGB(192,192,192)
		END IF	
		
	CASE "rama_sofitd"
		IF data = '0' THEN
		   this.object.rama_sofiin.BackGround.Color = RGB(255,255,255)
			this.object.rama_sofifi.BackGround.Color = RGB(255,255,255)
		ELSE
			this.SetItem(1,'rama_sofiin',integer(ls_Null))
			this.SetItem(1,'rama_sofifi',integer(ls_Null))
			this.object.rama_sofiin.BackGround.Color = RGB(192,192,192)
			this.object.rama_sofifi.BackGround.Color = RGB(192,192,192)
		END IF		
		
	CASE "rama_matstd"
		IF data = '0' THEN
		   this.object.rama_matsin.BackGround.Color = RGB(255,255,255)
			this.object.rama_matsfi.BackGround.Color = RGB(255,255,255)
		ELSE
			this.SetItem(1,'rama_matsin',integer(ls_Null))
			this.SetItem(1,'rama_matsfi',integer(ls_Null))
			this.object.rama_matsin.BackGround.Color = RGB(192,192,192)
			this.object.rama_matsfi.BackGround.Color = RGB(192,192,192)
		END IF			
	
		
	CASE "rama_nsemtd"
		IF data = '0' THEN
		   this.object.rama_nsemin.BackGround.Color = RGB(255,255,255)
			this.object.rama_nsemfi.BackGround.Color = RGB(255,255,255)
		ELSE
			this.SetItem(1,'rama_nsemin',integer(ls_Null))
			this.SetItem(1,'rama_nsemfi',integer(ls_Null))
			this.object.rama_nsemin.BackGround.Color = RGB(192,192,192)
			this.object.rama_nsemfi.BackGround.Color = RGB(192,192,192)
		END IF	

	CASE "rama_coactd"
		IF data = '0' THEN
		   this.object.rama_coacin.BackGround.Color = RGB(255,255,255)
			this.object.rama_coacfi.BackGround.Color = RGB(255,255,255)
		ELSE
			this.SetItem(1,'rama_coacin',integer(ls_Null))
			this.SetItem(1,'rama_coacfi',integer(ls_Null))
			this.object.rama_coacin.BackGround.Color = RGB(192,192,192)
			this.object.rama_coacfi.BackGround.Color = RGB(192,192,192)
			this.SetItem(1,'rama_coacle',0)
			this.SetItem(1,'rama_coacmo',0)
			this.SetItem(1,'rama_coacse',0)
		END IF	
		
	CASE "rama_comotd"
		IF data = '0' THEN
		   this.object.rama_comoin.BackGround.Color = RGB(255,255,255)
			this.object.rama_comofi.BackGround.Color = RGB(255,255,255)
		ELSE
			this.SetItem(1,'rama_comoin',integer(ls_Null))
			this.SetItem(1,'rama_comofi',integer(ls_Null))
			this.object.rama_comoin.BackGround.Color = RGB(192,192,192)
			this.object.rama_comofi.BackGround.Color = RGB(192,192,192)
			this.SetItem(1,'rama_comole',0)
			this.SetItem(1,'rama_comomo',0)
			this.SetItem(1,'rama_comose',0)
		END IF	
		
	CASE "rama_haritd"
		IF data = '0' THEN
		   this.object.rama_hariin.BackGround.Color = RGB(255,255,255)
			this.object.rama_harifi.BackGround.Color = RGB(255,255,255)
		ELSE
			this.SetItem(1,'rama_hariin',integer(ls_Null))
			this.SetItem(1,'rama_harifi',integer(ls_Null))
			this.object.rama_hariin.BackGround.Color = RGB(192,192,192)
			this.object.rama_harifi.BackGround.Color = RGB(192,192,192)
			this.SetItem(1,'rama_harile',0)
			this.SetItem(1,'rama_harimo',0)
			this.SetItem(1,'rama_harise',0)
		END IF	
		
	CASE "rama_grastd"
		IF data = '0' THEN
		   this.object.rama_grasin.BackGround.Color = RGB(255,255,255)
			this.object.rama_grasfi.BackGround.Color = RGB(255,255,255)
		ELSE
			this.SetItem(1,'rama_grasin',integer(ls_Null))
			this.SetItem(1,'rama_grasfi',integer(ls_Null))
			this.object.rama_grasin.BackGround.Color = RGB(192,192,192)
			this.object.rama_grasfi.BackGround.Color = RGB(192,192,192)
			this.SetItem(1,'rama_grasle',0)
			this.SetItem(1,'rama_grasmo',0)
			this.SetItem(1,'rama_grasse',0)
		END IF	
		
END CHOOSE
end event

type tp_2 from userobject within tab_1
event create ( )
event destroy ( )
string tag = "Antecedentes de Color de Fondo"
integer x = 18
integer y = 112
integer width = 3136
integer height = 900
long backcolor = 12632256
string text = "Color Fondo"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "UserObject!"
long picturemaskcolor = 553648127
dw_colorfondo dw_colorfondo
end type

on tp_2.create
this.dw_colorfondo=create dw_colorfondo
this.Control[]={this.dw_colorfondo}
end on

on tp_2.destroy
destroy(this.dw_colorfondo)
end on

type dw_colorfondo from datawindow within tp_2
integer x = 32
integer y = 36
integer width = 2999
integer height = 852
integer taborder = 10
boolean bringtotop = true
string dataobject = "dw_mant_mues_rangocolorfondo_sel"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_columna, ls_Null
Integer	li_color
Long		ll_FilaDano

SetNull(ls_Null)

ls_columna = GetColumnName()

CHOOSE CASE ls_columna
		
	CASE "raco_todos"
		
		this.SetItem(row,"raco_inicia",integer(ls_Null))
		this.SetItem(row,"raco_finali",integer(ls_Null))
		
	Case "raco_inicia"
		this.SetItem(row,"raco_todos",0)
		
	Case "raco_finali"
		this.SetItem(row,"raco_todos",0)
		
END CHOOSE
end event

event itemerror;RETURN 1
end event

event losefocus;Accepttext()
end event

event clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

type tp_3 from userobject within tab_1
event create ( )
event destroy ( )
string tag = "Color de Cubrimiento por Categoría"
integer x = 18
integer y = 112
integer width = 3136
integer height = 900
long backcolor = 12632256
string text = "Color Cubrimiento"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Custom072!"
long picturemaskcolor = 553648127
dw_cubrim dw_cubrim
end type

on tp_3.create
this.dw_cubrim=create dw_cubrim
this.Control[]={this.dw_cubrim}
end on

on tp_3.destroy
destroy(this.dw_cubrim)
end on

type dw_cubrim from datawindow within tp_3
integer x = 32
integer y = 36
integer width = 2999
integer height = 848
integer taborder = 10
boolean bringtotop = true
string dataobject = "dw_mant_mues_rangocolorcubri_sel"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event itemerror;RETURN 1
end event

event itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;Accepttext()
end event

event rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event itemchanged;Integer	li_Categoria, li_Nula
Long		ll_FilaDano

SetNull(li_Nula)

CHOOSE CASE dwo.Name

	CASE "racu_todos"
		
		this.SetItem(row,"racu_inicia",li_Nula)
		this.SetItem(row,"racu_finali",li_Nula)
		
	CASE "racU_inicia"
		
		this.SetItem(row,"raca_todos",0)
	
	CASE "racu_finali"
		
		this.SetItem(row,"raca_todos",0)
		

END CHOOSE
end event

type tp_4 from userobject within tab_1
event create ( )
event destroy ( )
string tag = "Distribución por Calibre"
integer x = 18
integer y = 112
integer width = 3136
integer height = 900
long backcolor = 12632256
string text = "Distribución Calibre"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "ArrangeIcons!"
long picturemaskcolor = 553648127
dw_calibre dw_calibre
end type

on tp_4.create
this.dw_calibre=create dw_calibre
this.Control[]={this.dw_calibre}
end on

on tp_4.destroy
destroy(this.dw_calibre)
end on

type dw_calibre from datawindow within tp_4
integer x = 32
integer y = 36
integer width = 2999
integer height = 848
integer taborder = 10
boolean bringtotop = true
string dataobject = "dw_mant_mues_rangodistribcalib_sel"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event itemerror;RETURN 1
end event

event itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF


end event

event losefocus;Accepttext()
end event

event rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event itemchanged;String	ls_Calibre
Long		ll_FilaDano
Integer	li_Nula

SetNull(li_Nula)

CHOOSE CASE dwo.Name
		
	CASE "raca_todos"
		
		this.SetItem(row,"raca_inicia",li_Nula)
		this.SetItem(row,"raca_finali",li_Nula)
		
	CASE "raca_inicia"
		
		this.SetItem(row,"raca_todos",0)
	
	CASE "raca_finali"
		
		this.SetItem(row,"raca_todos",0)
		
END CHOOSE
end event

